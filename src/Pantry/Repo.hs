{-# LANGUAGE NoImplicitPrelude   #-}
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


import Pantry.Types
import Pantry.Archive
import Pantry.Storage hiding (findOrGenerateCabalFile)
import RIO
import Path.IO (resolveFile')
import RIO.FilePath ((</>))
import RIO.Directory (doesDirectoryExist)
import RIO.ByteString (isInfixOf)
import RIO.ByteString.Lazy (toStrict)
import qualified RIO.Map as Map
import RIO.Process
import Database.Persist (Entity (..))
import qualified RIO.Text as T
import System.Console.ANSI (hSupportsANSIWithoutEmulation)
import System.IsWindows (osIsWindows)

data TarType = Gnu | Bsd

getGitTarType :: (HasProcessContext env, HasLogFunc env) => RIO env TarType
getGitTarType = if osIsWindows
  then do
    (_, stdoutBS, _) <- proc "git" ["--version"] readProcess
    let bs = toStrict stdoutBS
    -- If using Git for Windows, then assume that the tar type within
    -- `git submodule foreach <command>` is the Git-supplied\MSYS2-supplied
    -- GNU tar
    if "windows" `isInfixOf` bs then pure Gnu else getTarType
  else getTarType

getTarType :: (HasProcessContext env, HasLogFunc env) => RIO env TarType
getTarType = do
  (_, stdoutBS, _) <- proc "tar" ["--version"] readProcess
  let bs = toStrict stdoutBS
  pure $ if "GNU" `isInfixOf` bs then Gnu else Bsd

-- | Like 'fetchRepos', except with 'RawPackageMetadata' instead of 'PackageMetadata'.
--
-- @since 0.5.3
fetchReposRaw
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => [(Repo, RawPackageMetadata)]
  -> RIO env ()
fetchReposRaw pairs = do
  let repos = toAggregateRepos pairs
  logDebug (displayShow repos)
  for_ repos getRepos

-- | Fetch the given repositories at once and populate the pantry database.
--
-- @since 0.5.3
fetchRepos
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => [(Repo, PackageMetadata)]
  -> RIO env ()
fetchRepos pairs = do
  -- TODO be more efficient, group together shared archives
  fetchReposRaw $ map (second toRawPM) pairs

getRepoKey
  :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Repo
  -> RawPackageMetadata
  -> RIO env TreeKey
getRepoKey repo rpm = packageTreeKey <$> getRepo repo rpm -- potential optimization

getRepo
  :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Repo
  -> RawPackageMetadata
  -> RIO env Package
getRepo repo pm = do
  withCache $ getRepo' repo pm
  where
    withCache
      :: RIO env Package
      -> RIO env Package
    withCache inner = do
      mtid <- withStorage (loadRepoCache repo)
      case mtid of
        Just tid -> withStorage $ loadPackageById (RPLIRepo repo pm) tid
        Nothing -> do
          package <- inner
          withStorage $ do
            ment <- getTreeForKey $ packageTreeKey package
            case ment of
              Nothing -> error $ "invariant violated, Tree not found: " ++ show (packageTreeKey package)
              Just (Entity tid _) -> storeRepoCache repo (repoSubdir repo) tid
          pure package

getRepo'
  :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
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

getRepos
  :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => AggregateRepo
  -> RIO env [Package]
getRepos repo@(AggregateRepo (SimpleRepo{..}) repoSubdirs) =
  withCache getRepos'
  where
    withCache inner = do
      pkgs <- forM repoSubdirs $ \(subdir, rpm) -> withStorage $ do
        loadRepoCache (Repo sRepoUrl sRepoCommit sRepoType subdir) >>= \case
          Just tid -> fmap Right $ (, subdir) <$> loadPackageById (RPLIRepo (Repo sRepoUrl sRepoCommit sRepoType subdir) rpm) tid
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
                Nothing -> error $ "invariant violated, Tree not found: " ++ show (packageTreeKey package)
                Just (Entity tid _) -> storeRepoCache (Repo sRepoUrl sRepoCommit sRepoType subdir) subdir tid
            pure package
      pure (nubOrd ((fst <$> cachedPkgs) ++ newPkgs))

getRepos'
  :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
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
withRepoArchive
  :: forall env a. (HasLogFunc env, HasProcessContext env)
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
runGitCommand
  :: (HasLogFunc env, HasProcessContext env)
  => [String] -- ^ args
  -> RIO env ()
runGitCommand args =
  withModifyEnvVars go $
  void $ proc "git" args readProcess_
  where
    go = Map.delete "GIT_DIR"
       . Map.delete "GIT_CEILING_DIRECTORIES"
       . Map.delete "GIT_WORK_TREE"
       . Map.delete "GIT_INDEX_FILE"
       . Map.delete "GIT_OBJECT_DIRECTORY" -- possible optimization: set this to something Pantry controls
       . Map.delete "GIT_ALTERNATE_OBJECT_DIRECTORIES"

-- Include submodules files into the archive: use `git submodule
-- foreach` to execute `git archive` in each submodule and generate
-- tar archive. With bsd tar, the generated archive is extracted to a
-- temporary folder and the files in them are added to the tarball
-- referenced by the variable tarball in the haskell code. This is
-- done in GNU tar with -A option.
archiveSubmodules :: (HasLogFunc env, HasProcessContext env) => FilePath -> RIO env ()
archiveSubmodules tarball = do
  tarType <- getGitTarType
  let forceLocal =
          if osIsWindows
          then " --force-local "
          else mempty
  case tarType of
    Gnu -> runGitCommand
         [ "submodule", "foreach", "--recursive"
         , "git -c core.autocrlf=false archive --prefix=$displaypath/ -o bar.tar HEAD; "
           <> "tar" <> forceLocal <> " -Af " <> tarball <> " bar.tar"
         ]
    Bsd ->
       runGitCommand
          [ "submodule"
          , "foreach"
          , "--recursive"
          , "git -c core.autocrlf=false archive --prefix=$displaypath/ -o bar.tar HEAD;" <>
            " rm -rf temp; mkdir temp; mv bar.tar temp/; tar " <>
            " -C temp -xf temp/bar.tar; " <>
            "rm temp/bar.tar; tar " <>
            " -C temp -rf " <>
            tarball <>
            " . ;"
          ]

-- | Run an hg command
runHgCommand
  :: (HasLogFunc env, HasProcessContext env)
  => [String] -- ^ args
  -> RIO env ()
runHgCommand args = void $ proc "hg" args readProcess_

-- | Create a tarball containing files from a repository
createRepoArchive ::
     forall env. (HasLogFunc env, HasProcessContext env)
  => SimpleRepo
  -> FilePath -- ^ Output tar archive filename
  -> RIO env ()
createRepoArchive sr tarball = do
  withRepo sr $
    case sRepoType sr of
      RepoGit -> do
        runGitCommand
          ["-c", "core.autocrlf=false", "archive", "-o", tarball, "HEAD"]
        archiveSubmodules tarball
      RepoHg -> runHgCommand ["archive", tarball, "-X", ".hg_archival.txt"]


-- | Clone the repository and execute the action with the working
-- directory set to the repository root.
--
-- @since 0.1.0.0
withRepo
  :: forall env a. (HasLogFunc env, HasProcessContext env)
  => SimpleRepo
  -> RIO env a
  -> RIO env a
withRepo sr@SimpleRepo{..} action =
  withSystemTempDirectory "with-repo" $ \tmpDir -> do
    -- Note we do not immediately change directories into the new temporary directory,
    -- but instead wait until we have finished cloning the repo. This is because the
    -- repo URL may be a relative path on the local filesystem, and we should interpret
    -- it as relative to the current directory, not the temporary directory.
    let dir = tmpDir </> "cloned"
        (runCommand, resetArgs, submoduleArgs) =
          case sRepoType of
            RepoGit ->
              ( runGitCommand
              , ["reset", "--hard", T.unpack sRepoCommit]
              , Just ["submodule", "update", "--init", "--recursive"]
              )
            RepoHg ->
              ( runHgCommand
              , ["update", "-C", T.unpack sRepoCommit]
              , Nothing
              )
        fixANSIForWindows =
          -- On Windows 10, an upstream issue with the `git clone` command means that
          -- command clears, but does not then restore, the
          -- ENABLE_VIRTUAL_TERMINAL_PROCESSING flag for native terminals. The
          -- following hack re-enables the lost ANSI-capability.
          when osIsWindows $ void $ liftIO $ hSupportsANSIWithoutEmulation stdout

    logInfo $ "Cloning " <> display sRepoCommit <> " from " <> display sRepoUrl
    runCommand ["clone", T.unpack sRepoUrl, dir]
    fixANSIForWindows
    created <- doesDirectoryExist dir
    unless created $ throwIO $ FailedToCloneRepo sr

    withWorkingDir dir $ do
      runCommand resetArgs
      traverse_ runCommand submoduleArgs
      fixANSIForWindows
      action
