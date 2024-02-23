{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Pantry.Repo
  ( fetchReposRaw
  , fetchRepos
  , getRepo
  , getRepoKey
  , createRepoArchive
  , withRepoArchive
  , withRepo
  ) where

import           Database.Persist.Class.PersistEntity ( Entity (..) )
import           Pantry.Archive ( getArchivePackage )
import           Pantry.Storage
                   ( getTreeForKey, loadPackageById, loadRepoCache
                   , storeRepoCache, withStorage
                   )
import           Pantry.Types
                   ( AggregateRepo (..), ArchiveLocation (..), HasPantryConfig
                   , Package (..), PackageMetadata (..), PantryException (..)
                   , RawArchive (..), RawPackageLocationImmutable (..)
                   , RawPackageMetadata (..), RelFilePath (..), Repo (..)
                   , RepoType (..), ResolvedPath (..), SimpleRepo (..)
                   , TreeKey (..), arToSimpleRepo, rToSimpleRepo
                   , toAggregateRepos, toRawPM
                   )
import           Path.IO ( resolveFile' )
import           RIO
import           RIO.ByteString ( isInfixOf )
import qualified RIO.ByteString.Lazy as BL
import           RIO.Directory ( doesDirectoryExist )
import           RIO.FilePath ( (</>) )
import qualified RIO.Map as Map
import           RIO.Process
                   ( ExitCodeException (..), HasProcessContext, ProcessConfig
                   , byteStringInput, proc, readProcess, readProcess_
                   , readProcessStdout_, runProcess_, setStdin
                   , withModifyEnvVars, withWorkingDir
                   )
import qualified RIO.Text as T
#if MIN_VERSION_ansi_terminal(1, 0, 2)
import           System.Console.ANSI ( hNowSupportsANSI )
#else
import           System.Console.ANSI ( hSupportsANSIWithoutEmulation )
#endif
import           System.IsWindows ( osIsWindows )

data TarType = Gnu | Bsd

getTarType :: (HasProcessContext env, HasLogFunc env) => RIO env TarType
getTarType = do
  (_, stdoutBS, _) <- proc "tar" ["--version"] readProcess
  let bs = BL.toStrict stdoutBS
  pure $ if "GNU" `isInfixOf` bs then Gnu else Bsd

-- | Like 'fetchRepos', except with 'RawPackageMetadata' instead of
-- 'PackageMetadata'.
--
-- @since 0.5.3
fetchReposRaw ::
     (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => [(Repo, RawPackageMetadata)]
  -> RIO env ()
fetchReposRaw pairs = do
  let repos = toAggregateRepos pairs
  logDebug (displayShow repos)
  for_ repos getRepos

-- | Fetch the given repositories at once and populate the pantry database.
--
-- @since 0.5.3
fetchRepos ::
     (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => [(Repo, PackageMetadata)]
  -> RIO env ()
fetchRepos pairs = do
  -- TODO be more efficient, group together shared archives
  fetchReposRaw $ map (second toRawPM) pairs

getRepoKey ::
     forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Repo
  -> RawPackageMetadata
  -> RIO env TreeKey
getRepoKey repo rpm = packageTreeKey <$> getRepo repo rpm -- potential optimization

getRepo ::
     forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Repo
  -> RawPackageMetadata
  -> RIO env Package
getRepo repo pm = do
  withCache $ getRepo' repo pm
 where
  withCache :: RIO env Package -> RIO env Package
  withCache inner = do
    mtid <- withStorage (loadRepoCache repo)
    case mtid of
      Just tid -> withStorage $ loadPackageById (RPLIRepo repo pm) tid
      Nothing -> do
        package <- inner
        withStorage $ do
          ment <- getTreeForKey $ packageTreeKey package
          case ment of
            Nothing ->  error $
                "invariant violated, Tree not found: "
              ++ show (packageTreeKey package)
            Just (Entity tid _) -> storeRepoCache repo (repoSubdir repo) tid
        pure package

getRepo' ::
     forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Repo
  -> RawPackageMetadata
  -> RIO env Package
getRepo' repo@Repo{..} rpm = do
  withRepoArchive (rToSimpleRepo repo) $ \tarball -> do
    abs' <- resolveFile' tarball
    getArchivePackage
      (RPLIRepo repo rpm)
      RawArchive
        { raLocation = ALFilePath $ ResolvedPath
            { resolvedRelative = RelFilePath $ T.pack tarball
            , resolvedAbsolute = abs'
            }
        , raHash = Nothing
        , raSize = Nothing
        , raSubdir = repoSubdir
        }
      rpm

getRepos ::
     forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => AggregateRepo
  -> RIO env [Package]
getRepos repo@(AggregateRepo (SimpleRepo{..}) repoSubdirs) = withCache getRepos'
 where
  withCache inner = do
    pkgs <- forM repoSubdirs $ \(subdir, rpm) -> withStorage $ do
      loadRepoCache (Repo sRepoUrl sRepoCommit sRepoType subdir) >>= \case
        Just tid ->
          fmap Right $ (, subdir) <$> loadPackageById (RPLIRepo (Repo sRepoUrl sRepoCommit sRepoType subdir) rpm) tid
        Nothing  -> pure $ Left (subdir, rpm)
    let (missingPkgs, cachedPkgs) = partitionEithers pkgs
    newPkgs <-
      if null missingPkgs
      then pure []
      else do
        packages <- inner repo { aRepoSubdirs = missingPkgs }
        forM packages $ \(package, subdir) -> do
          withStorage $ do
            ment <- getTreeForKey $ packageTreeKey package
            case ment of
              Nothing -> error $
                   "invariant violated, Tree not found: "
                ++ show (packageTreeKey package)
              Just (Entity tid _) ->
                storeRepoCache (Repo sRepoUrl sRepoCommit sRepoType subdir) subdir tid
          pure package
    pure (nubOrd ((fst <$> cachedPkgs) ++ newPkgs))

getRepos' ::
     forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => AggregateRepo
  -> RIO env [(Package, Text)] -- ^ [(package, subdir)]
getRepos' ar@(AggregateRepo (SimpleRepo{..}) repoSubdirs) = do
  withRepoArchive (arToSimpleRepo ar) $ \tarball -> do
    abs' <- resolveFile' tarball
    forM repoSubdirs $ \(subdir, rpm) -> do
      (,subdir) <$> getArchivePackage
        (RPLIRepo (Repo sRepoUrl sRepoCommit sRepoType subdir) rpm)
        RawArchive
          { raLocation = ALFilePath $ ResolvedPath
              { resolvedRelative = RelFilePath $ T.pack tarball
              , resolvedAbsolute = abs'
              }
          , raHash = Nothing
          , raSize = Nothing
          , raSubdir = subdir
          }
        rpm

-- | Fetch a repository and create a (temporary) tar archive from it. Pass the
-- path of the generated tarball to the given action.
withRepoArchive ::
     forall env a. (HasLogFunc env, HasProcessContext env)
  => SimpleRepo
  -> (FilePath -> RIO env a)
  -> RIO env a
withRepoArchive sr action =
  withSystemTempDirectory "with-repo-archive" $ \tmpdirArchive -> do
    let tarball = tmpdirArchive </> "foo.tar"
    createRepoArchive sr tarball
    action tarball

-- | Run a git command, setting appropriate environment variable settings. See
-- <https://github.com/commercialhaskell/stack/issues/3748>.
runGitCommand ::
     (HasLogFunc env, HasProcessContext env)
  => [String] -- ^ args
  -> RIO env ()
runGitCommand = runGitProcess runProcess_

-- | Run a git command, setting appropriate environment variable settings. See
-- <https://github.com/commercialhaskell/stack/issues/3748>.
runGitCommandStdout ::
     (HasLogFunc env, HasProcessContext env)
  => [String] -- ^ args
  -> RIO env BL.ByteString
runGitCommandStdout = runGitProcess readProcessStdout_

-- | Run a git command, setting appropriate environment variable settings. See
-- <https://github.com/commercialhaskell/stack/issues/3748>.
runGitProcess ::
     (HasLogFunc env, HasProcessContext env)
  => (ProcessConfig () () () -> RIO env a)
  -> [String] -- ^ args
  -> RIO env a
runGitProcess inner args =
  withModifyEnvVars go $ proc "git" args inner
 where
  go = Map.delete "GIT_DIR"
     . Map.delete "GIT_CEILING_DIRECTORIES"
     . Map.delete "GIT_WORK_TREE"
     . Map.delete "GIT_INDEX_FILE"
     . Map.delete "GIT_OBJECT_DIRECTORY" -- possible optimization: set this to something Pantry controls
     . Map.delete "GIT_ALTERNATE_OBJECT_DIRECTORIES"

-- | Run an hg command
runHgCommand ::
     (HasLogFunc env, HasProcessContext env)
  => [String] -- ^ args
  -> RIO env ()
runHgCommand args = void $ proc "hg" args readProcess_

-- | Create a tarball containing files from a repository.
--
-- For a Git repository, the files will be archived with @core.autocrlf=false@
-- and @core.eol=lf@. That is, files marked as text in the respository will have
-- LF line endings unless a different line ending in the working tree is
-- specified for the file in the repository.
createRepoArchive ::
     forall env. (HasLogFunc env, HasProcessContext env)
  => SimpleRepo
  -> FilePath -- ^ Output tar archive filename
  -> RIO env ()
createRepoArchive sr tarball = do
  withRepo' True sr $
    case sRepoType sr of
      RepoGit -> do
        tarType <- getTarType
        let (addForceLocal, addVerbatimFilesFrom) = case tarType of
              Gnu ->
                ( if osIsWindows then ("--force-local" :) else id
                , ("--verbatim-files-from" :)
                )
              Bsd -> (id, id)
            tarArgs =
              addForceLocal $ "-caf" : tarball : addVerbatimFilesFrom ["-T-"]
        files <- runGitCommandStdout [ "ls-files", "--recurse-submodules" ]
        proc "tar" tarArgs $ runProcess_ . setStdin (byteStringInput files)
      RepoHg -> runHgCommand ["archive", tarball, "-X", ".hg_archival.txt"]

-- | Clone the repository (and, in the case of Git and if necessary, fetch the
-- specific commit) and execute the action with the working directory set to the
-- repository root. For Git repositories, respects the @core.autocrlf@ and
-- @core.eol@ settings.
--
-- @since 0.1.0.0
withRepo ::
     forall env a. (HasLogFunc env, HasProcessContext env)
  => SimpleRepo
  -> RIO env a
  -> RIO env a
withRepo = withRepo' False

-- | Clone the repository (and, in the case of Git and if necessary, fetch the
-- specific commit) and execute the action with the working directory set to the
-- repository root.
--
-- @since 0.1.0.0
withRepo' ::
     forall env a. (HasLogFunc env, HasProcessContext env)
  => Bool
     -- ^ When using Git, pass @-c core.autocrlf=false@ and @-c core.eol=lf@
     -- when cloning the respository?
  -> SimpleRepo
  -> RIO env a
  -> RIO env a
withRepo' disableAutoCrLf sr@SimpleRepo{..} action =
  withSystemTempDirectory "with-repo" $ \tmpDir -> do
    let repoUrl = T.unpack sRepoUrl
        repoCommit = T.unpack sRepoCommit
        dir = tmpDir </> "cloned"
        (runCommand, cloneArgs, resetArgs) =
          case sRepoType of
            RepoGit ->
              ( runGitCommand
              , lineEndingsArgs <> ["clone", repoUrl, dir]
              , lineEndingsArgs <> ["reset", "--hard", repoCommit]
              )
            RepoHg ->
              ( runHgCommand
              , ["clone", repoUrl, dir]
              , ["update", "-C", repoCommit]
              )
        fetchCommit = ["fetch", repoUrl, repoCommit]
        lineEndingsArgs = if disableAutoCrLf
          then ["-c", "core.autocrlf=false", "-c", "core.eol=lf"]
          else []
        submoduleArgs =
          lineEndingsArgs <> ["submodule", "update", "--init", "--recursive"]
        fixANSIForWindows =
          -- On Windows 10, an upstream issue with the `git clone` command means
          -- that command clears, but does not then restore, the
          -- ENABLE_VIRTUAL_TERMINAL_PROCESSING flag for native terminals. The
          -- following hack re-enables the lost ANSI-capability.
          when osIsWindows $ void $ liftIO $
#if MIN_VERSION_ansi_terminal(1, 0, 2)
            hNowSupportsANSI stdout
#else
            hSupportsANSIWithoutEmulation stdout
#endif
    logInfo $ "Cloning " <> display sRepoCommit <> " from " <> display sRepoUrl
    runCommand cloneArgs
    fixANSIForWindows
    created <- doesDirectoryExist dir
    unless created $ throwIO $ FailedToCloneRepo sr

    -- Note we do not immediately change directories into the new temporary
    -- directory, but instead wait until we have finished cloning the repo. This
    -- is because the repo URL may be a relative path on the local filesystem,
    -- and we should interpret it as relative to the current directory, not the
    -- temporary directory.
    withWorkingDir dir $ do
      case sRepoType of
        RepoGit -> do
          catch
            -- This will result in a failure exit code if the specified commit
            -- is not in the clone of the repository.
            (runCommand resetArgs)
            ( \(_ :: ExitCodeException) -> do
               -- Perhaps the specified commit is not one that is brought across
               -- by `git clone`. For example, in the case of a GitHub
               -- repository, it may be a commit from a different repository
               -- that is the subject of an unmerged pull request. Try to fetch
               -- the specific commit and then try again.
               runCommand fetchCommit
               runCommand resetArgs
            )
          runCommand submoduleArgs
          fixANSIForWindows
        RepoHg -> runCommand resetArgs
      action
