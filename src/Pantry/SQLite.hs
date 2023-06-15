{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pantry.SQLite
  ( Storage (..)
  , initStorage
  ) where

import           Database.Persist.Sqlite
import           Pantry.Internal.Companion
import           Pantry.Types
                   ( PantryException (MigrationFailure), Storage (..) )
import           Path ( Abs, File, Path, parent, toFilePath )
import           Path.IO ( ensureDir )
import           RIO hiding ( FilePath )
import           RIO.Orphans ()
import           System.FileLock
                   ( SharedExclusive (..), withFileLock, withTryFileLock )

initStorage ::
     HasLogFunc env
  => Text
  -> Migration
  -> Path Abs File -- ^ storage file
  -> (Storage -> RIO env a)
  -> RIO env a
initStorage description migration fp inner = do
  ensureDir $ parent fp

  migrates <- withWriteLock (display description) fp $ wrapMigrationFailure $
    withSqliteConnInfo (sqinfo True) $ runSqlConn $
    runMigrationSilent migration
  forM_ migrates $ \mig -> logDebug $ "Migration executed: " <> display mig

  -- Make a single connection to the SQLite database and wrap it in an MVar for
  -- the entire execution context. Previously we used a resource pool of size
  -- 1, but (1) there's no advantage to that, and (2) it had a _very_ weird
  -- interaction with Docker on OS X where when resource-pool's reaper would
  -- trigger, it would somehow cause the Stack process inside the container to
  -- die with a SIGBUS. Definitely an interesting thing worth following up
  -- on...
  withSqliteConnInfo (sqinfo False) $ \conn0 -> do
    connVar <- newMVar conn0
    inner $ Storage
      -- NOTE: Currently, we take a write lock on every action. This is
      -- a bit heavyweight, but it avoids the SQLITE_BUSY errors
      -- reported in
      -- <https://github.com/commercialhaskell/stack/issues/4471>
      -- completely. We can investigate more elegant solutions in the
      -- future, such as separate read and write actions or introducing
      -- smarter retry logic.
      { withStorage_ = \action -> withMVar connVar $ \conn ->
                       withWriteLock (display description) fp $
                       runSqlConn action conn
      , withWriteLock_ = id
      }
 where
  wrapMigrationFailure = handleAny (throwIO . MigrationFailure description fp)

  sqinfo isMigration
    = set extraPragmas ["PRAGMA busy_timeout=2000;"]
    $ set walEnabled False

    -- When doing a migration, we want to disable foreign key checking, since
    -- the order in which tables are created by the migration scripts may not
    -- respect foreign keys. The rest of the time: enforce those foreign keys.
    $ set fkEnabled (not isMigration)

    $ mkSqliteConnectionInfo (fromString $ toFilePath fp)

-- | Ensure that only one process is trying to write to the database at a time.
-- See https://github.com/commercialhaskell/stack/issues/4471 and comments
-- above.
withWriteLock ::
     HasLogFunc env
  => Utf8Builder -- ^ database description, for lock messages
  -> Path Abs File -- ^ SQLite database file
  -> RIO env a
  -> RIO env a
withWriteLock desc dbFile inner = do
  let lockFile = toFilePath dbFile ++ ".pantry-write-lock"
  withRunInIO $ \run -> do
    mres <- withTryFileLock lockFile Exclusive $ const $ run inner
    case mres of
      Just res -> pure res
      Nothing -> do
        let complainer :: Companion IO
            complainer delay = run $ do
              -- Wait five seconds before giving the first message to
              -- avoid spamming the user for uninteresting file locks
              delay $ 5 * 1000 * 1000 -- 5 seconds
              logInfo $
                   "Unable to get a write lock on the "
                <> desc
                <> " database, waiting..."

              -- Now loop printing a message every 1 minute
              forever $ do
                delay (60 * 1000 * 1000) -- 1 minute
                  `onCompanionDone` logInfo
                    (  "Acquired the "
                    <> desc
                    <> " database write lock"
                    )
                logWarn
                  (  "Still waiting on the "
                  <> desc
                  <> " database write lock..."
                  )
        withCompanion complainer $ \stopComplaining ->
          withFileLock lockFile Exclusive $ const $ do
            stopComplaining
            run inner
