{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Content addressable Haskell package management, providing for
-- secure, reproducible acquisition of Haskell package contents and
-- metadata.
--
-- @since 0.1.0.0
module Pantry
  ( -- * Running
    PantryConfig
  , HackageSecurityConfig (..)
  , defaultHackageSecurityConfig
  , defaultCasaRepoPrefix
  , defaultCasaMaxPerRequest
  , HasPantryConfig (..)
  , withPantryConfig
  , HpackExecutable (..)

    -- ** Convenience
  , PantryApp
  , runPantryApp
  , runPantryAppClean
  , runPantryAppWith
  , hpackExecutableL

    -- * Types

    -- ** Exceptions
  , PantryException (..)

    -- ** Cabal types
  , PackageName
  , Version
  , FlagName
  , PackageIdentifier (..)

    -- ** Files
  , FileSize (..)
  , RelFilePath (..)
  , ResolvedPath (..)
  , Unresolved

    -- ** Cryptography
  , SHA256
  , TreeKey (..)
  , BlobKey (..)

    -- ** Packages
  , RawPackageMetadata (..)
  , PackageMetadata (..)
  , Package (..)

    -- ** Hackage
  , CabalFileInfo (..)
  , Revision (..)
  , PackageIdentifierRevision (..)
  , UsePreferredVersions (..)

    -- ** Archives
  , RawArchive (..)
  , Archive (..)
  , ArchiveLocation (..)

    -- ** Repos
  , Repo (..)
  , RepoType (..)
  , withRepo

    -- ** Package location
  , RawPackageLocation (..)
  , PackageLocation (..)
  , toRawPL
  , RawPackageLocationImmutable (..)
  , PackageLocationImmutable (..)

    -- ** Snapshots
  , RawSnapshotLocation (..)
  , SnapshotLocation (..)
  , toRawSL
  , RawSnapshot (..)
  , Snapshot (..)
  , RawSnapshotPackage (..)
  , SnapshotPackage (..)
  , RawSnapshotLayer (..)
  , SnapshotLayer (..)
  , toRawSnapshotLayer
  , WantedCompiler (..)

    -- * Loading values
  , resolvePaths
  , loadPackageRaw
  , tryLoadPackageRawViaCasa
  , loadPackage
  , loadRawSnapshotLayer
  , loadSnapshotLayer
  , loadSnapshot
  , loadAndCompleteSnapshot
  , loadAndCompleteSnapshotRaw
  , CompletedSL (..)
  , CompletedPLI (..)
  , addPackagesToSnapshot
  , AddPackagesConfig (..)

    -- * Completion functions
  , completePackageLocation
  , completeSnapshotLayer
  , completeSnapshotLocation

    -- * Parsers
  , parseWantedCompiler
  , parseRawSnapshotLocation
  , parsePackageIdentifierRevision
  , parseHackageText

    -- ** Cabal values
  , parsePackageIdentifier
  , parsePackageName
  , parsePackageNameThrowing
  , parseFlagName
  , parseVersion
  , parseVersionThrowing

    -- * Stackage snapshots
  , ltsSnapshotLocation
  , nightlySnapshotLocation

    -- * Cabal helpers
  , packageIdentifierString
  , packageNameString
  , flagNameString
  , versionString
  , moduleNameString
  , CabalString (..)
  , toCabalStringMap
  , unCabalStringMap
  , gpdPackageIdentifier
  , gpdPackageName
  , gpdVersion

    -- * Package location
  , fetchPackages
  , unpackPackageLocationRaw
  , unpackPackageLocation
  , getPackageLocationName
  , getRawPackageLocationIdent
  , packageLocationIdent
  , packageLocationVersion
  , getRawPackageLocationTreeKey
  , getPackageLocationTreeKey

    -- * Cabal files
  , loadCabalFileRaw
  , loadCabalFile
  , loadCabalFileRawImmutable
  , loadCabalFileImmutable
  , loadCabalFilePath
  , findOrGenerateCabalFile
  , PrintWarnings (..)

    -- * Hackage index
  , updateHackageIndex
  , DidUpdateOccur (..)
  , RequireHackageIndex (..)
  , hackageIndexTarballL
  , getHackagePackageVersions
  , getLatestHackageVersion
  , getLatestHackageLocation
  , getLatestHackageRevision
  , getHackageTypoCorrections
  , loadGlobalHints
  , partitionReplacedDependencies
    -- * Snapshot cache
  , SnapshotCacheHash (..)
  , withSnapshotCache
  ) where

import Database.Persist (entityKey)
import RIO
import Conduit
import Control.Arrow (right)
import Control.Monad.State.Strict (State, execState, get, modify')
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.ByteString as B
import qualified RIO.Text as T
import qualified RIO.List as List
import qualified RIO.FilePath as FilePath
import Pantry.Archive
import Pantry.Casa
import Casa.Client (thParserCasaRepo, CasaRepoPrefix)
import Pantry.Repo
import qualified Pantry.SHA256 as SHA256
import Pantry.Storage hiding (TreeEntry, PackageName, Version)
import Pantry.Tree
import Pantry.Types as P
import Pantry.Hackage
import Path (Path, Abs, File, toFilePath, Dir, (</>), filename, parseAbsDir, parent, parseRelFile)
import Path.IO (doesFileExist, resolveDir', listDir)
import Distribution.PackageDescription (GenericPackageDescription, FlagName)
import qualified Distribution.PackageDescription as D
import Distribution.Parsec (PWarning (..), showPos)
import qualified Hpack
import qualified Hpack.Config as Hpack
import Network.HTTP.Download
import RIO.PrettyPrint
import RIO.PrettyPrint.StylesUpdate
import RIO.Process
import RIO.Directory (getAppUserDataDirectory)
import qualified Data.Yaml as Yaml
import Pantry.Internal.AesonExtended (WithJSONWarnings (..), Value)
import Data.Aeson.Types (parseEither)
import Data.Monoid (Endo (..))
import Pantry.HTTP
import Data.Char (isHexDigit)
import Data.Time (getCurrentTime, diffUTCTime)

-- | Create a new 'PantryConfig' with the given settings.
--
-- For something easier to use in simple cases, see 'runPantryApp'.
--
-- @since 0.1.0.0
withPantryConfig
  :: HasLogFunc env
  => Path Abs Dir
  -- ^ pantry root directory, where the SQLite database and Hackage
  -- downloads are kept.
  -> HackageSecurityConfig
  -- ^ Hackage configuration. You probably want
  -- 'defaultHackageSecurityConfig'.
  -> HpackExecutable
  -- ^ When converting an hpack @package.yaml@ file to a cabal file,
  -- what version of hpack should we use?
  -> Int
  -- ^ Maximum connection count
  -> CasaRepoPrefix
  -- ^ The casa pull URL e.g. https://casa.fpcomplete.com/v1/pull.
  -> Int
  -- ^ Max casa keys to pull per request.
  -> (PantryConfig -> RIO env a)
  -- ^ What to do with the config
  -> RIO env a
withPantryConfig root hsc he count pullURL maxPerRequest inner = do
  env <- ask
  pantryRelFile <- parseRelFile "pantry.sqlite3"
  -- Silence persistent's logging output, which is really noisy
  runRIO (mempty :: LogFunc) $ initStorage (root </> pantryRelFile) $ \storage -> runRIO env $ do
    ur <- newMVar True
    ref1 <- newIORef mempty
    ref2 <- newIORef mempty
    inner PantryConfig
      { pcHackageSecurity = hsc
      , pcHpackExecutable = he
      , pcRootDir = root
      , pcStorage = storage
      , pcUpdateRef = ur
      , pcConnectionCount = count
      , pcParsedCabalFilesRawImmutable = ref1
      , pcParsedCabalFilesMutable = ref2
      , pcCasaRepoPrefix = pullURL
      , pcCasaMaxPerRequest = maxPerRequest
      }

-- | Default pull URL for Casa.
--
-- @since 0.1.1.1
defaultCasaRepoPrefix :: CasaRepoPrefix
defaultCasaRepoPrefix = $(thParserCasaRepo "https://casa.fpcomplete.com")

-- | Default max keys to pull per request.
--
-- @since 0.1.1.1
defaultCasaMaxPerRequest :: Int
defaultCasaMaxPerRequest = 1280

-- | Default 'HackageSecurityConfig' value using the official Hackage server.
--
-- @since 0.1.0.0
defaultHackageSecurityConfig :: HackageSecurityConfig
defaultHackageSecurityConfig = HackageSecurityConfig
  { hscKeyIds =
      [ "0a5c7ea47cd1b15f01f5f51a33adda7e655bc0f0b0615baa8e271f4c3351e21d"
      , "1ea9ba32c526d1cc91ab5e5bd364ec5e9e8cb67179a471872f6e26f0ae773d42"
      , "280b10153a522681163658cb49f632cde3f38d768b736ddbc901d99a1a772833"
      , "2a96b1889dc221c17296fcc2bb34b908ca9734376f0f361660200935916ef201"
      , "2c6c3627bd6c982990239487f1abd02e08a02e6cf16edb105a8012d444d870c3"
      , "51f0161b906011b52c6613376b1ae937670da69322113a246a09f807c62f6921"
      , "772e9f4c7db33d251d5c6e357199c819e569d130857dc225549b40845ff0890d"
      , "aa315286e6ad281ad61182235533c41e806e5a787e0b6d1e7eef3f09d137d2e9"
      , "fe331502606802feac15e514d9b9ea83fee8b6ffef71335479a2e68d84adc6b0"
      ]
  , hscKeyThreshold = 3
  , hscDownloadPrefix = "https://s3.amazonaws.com/hackage.fpcomplete.com/"
  , hscIgnoreExpiry = False
  }

-- | Returns the latest version of the given package available from
-- Hackage.
--
-- @since 0.1.0.0
getLatestHackageVersion
  :: (HasPantryConfig env, HasLogFunc env)
  => RequireHackageIndex
  -> PackageName -- ^ package name
  -> UsePreferredVersions
  -> RIO env (Maybe PackageIdentifierRevision)
getLatestHackageVersion req name preferred =
  ((fmap fst . Map.maxViewWithKey) >=> go) <$> getHackagePackageVersions req preferred name
  where
    go (version, m) = do
      (_rev, BlobKey sha size) <- fst <$> Map.maxViewWithKey m
      pure $ PackageIdentifierRevision name version $ CFIHash sha $ Just size

-- | Returns location of the latest version of the given package available from
-- Hackage.
--
-- @since 0.1.0.0
getLatestHackageLocation
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => RequireHackageIndex
  -> PackageName -- ^ package name
  -> UsePreferredVersions
  -> RIO env (Maybe PackageLocationImmutable)
getLatestHackageLocation req name preferred = do
  mversion <-
    fmap fst . Map.maxViewWithKey <$> getHackagePackageVersions req preferred name
  let mVerCfKey = do
        (version, revisions) <- mversion
        (_rev, cfKey) <- fst <$> Map.maxViewWithKey revisions
        pure (version, cfKey)

  forM mVerCfKey $ \(version, cfKey@(BlobKey sha size)) -> do
    let pir = PackageIdentifierRevision name version (CFIHash sha (Just size))
    treeKey' <- getHackageTarballKey pir
    pure $ PLIHackage (PackageIdentifier name version) cfKey treeKey'

-- | Returns the latest revision of the given package version available from
-- Hackage.
--
-- @since 0.1.0.0
getLatestHackageRevision
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => RequireHackageIndex
  -> PackageName -- ^ package name
  -> Version
  -> RIO env (Maybe (Revision, BlobKey, TreeKey))
getLatestHackageRevision req name version = do
  revisions <- getHackagePackageVersionRevisions req name version
  case fmap fst $ Map.maxViewWithKey revisions of
    Nothing -> pure Nothing
    Just (revision, cfKey@(BlobKey sha size)) -> do
      let cfi = CFIHash sha (Just size)
      treeKey' <- getHackageTarballKey (PackageIdentifierRevision name version cfi)
      return $ Just (revision, cfKey, treeKey')

-- | Fetch keys and blobs and insert into the database where possible.
fetchTreeKeys ::
     (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => [RawPackageLocationImmutable]
  -> RIO env ()
fetchTreeKeys treeKeys = do
  pure ()
  -- Find all tree keys that are missing from the database.
  packageLocationsMissing :: [RawPackageLocationImmutable] <-
    withStorage
      (filterM
         (fmap isNothing . maybe (pure Nothing) getTreeForKey . getRawTreeKey)
         treeKeys)
  pullTreeStart <- liftIO getCurrentTime
  -- Pull down those tree keys from Casa, automatically inserting into
  -- our local database.
  treeKeyBlobs :: Map TreeKey P.Tree <-
    fmap
      Map.fromList
      (withStorage
         (runConduitRes
            (casaBlobSource
               (fmap unTreeKey (mapMaybe getRawTreeKey packageLocationsMissing)) .|
             mapMC parseTreeM .|
             sinkList)))
  pullTreeEnd <- liftIO getCurrentTime
  let pulledPackages =
        mapMaybe
          (\treeKey' ->
             List.find
               ((== Just treeKey') . getRawTreeKey)
               packageLocationsMissing)
          (Map.keys treeKeyBlobs)
  -- Pull down all unique file blobs.
  let uniqueFileBlobKeys :: Set BlobKey
      uniqueFileBlobKeys =
        foldMap
          (\(P.TreeMap files) -> Set.fromList (map teBlob (toList files)))
          treeKeyBlobs
  pullBlobStart <- liftIO getCurrentTime
  pulledBlobKeys :: Int <-
    withStorage
      (runConduitRes
         (casaBlobSource uniqueFileBlobKeys .| mapC (const 1) .| sumC))
  pullBlobEnd <- liftIO getCurrentTime
  logDebug
    ("Pulled from Casa: " <>
     mconcat (List.intersperse ", " (map display pulledPackages)) <>
     " (" <>
     display (T.pack (show (diffUTCTime pullTreeEnd pullTreeStart))) <>
     "), " <>
     plural pulledBlobKeys "file" <>
     " (" <>
     display (T.pack (show (diffUTCTime pullBlobEnd pullBlobStart))) <>
     ")")
  -- Store the tree for each missing package.
  for_
    packageLocationsMissing
    (\rawPackageLocationImmutable ->
       let mkey = getRawTreeKey rawPackageLocationImmutable
        in case mkey of
             Nothing ->
               logDebug
                 ("Ignoring package with no tree key " <>
                  display rawPackageLocationImmutable <>
                  ", can't look in Casa for it.")
             Just key ->
               case Map.lookup key treeKeyBlobs of
                 Nothing ->
                   logDebug
                     ("Package key " <> display key <> " (" <>
                      display rawPackageLocationImmutable <>
                      ") not returned from Casa.")
                 Just tree -> do
                   identifier <-
                     getRawPackageLocationIdent rawPackageLocationImmutable
                   case findCabalOrHpackFile rawPackageLocationImmutable tree of
                     Just buildFile ->
                       void
                         (withStorage
                            (storeTree
                               rawPackageLocationImmutable
                               identifier
                               tree
                               buildFile))
                     Nothing ->
                       logWarn
                         ("Unable to find build file for package: " <>
                          display rawPackageLocationImmutable))
  where
    unTreeKey :: TreeKey -> BlobKey
    unTreeKey (P.TreeKey blobKey) = blobKey

-- | Download all of the packages provided into the local cache
-- without performing any unpacking. Can be useful for build tools
-- wanting to prefetch or provide an offline mode.
--
-- @since 0.1.0.0
fetchPackages
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env, Foldable f)
  => f PackageLocationImmutable
  -> RIO env ()
fetchPackages pls = do
    fetchTreeKeys (fmap toRawPLI (toList pls))
    traverseConcurrently_ (void . uncurry getHackageTarball) hackages
    -- TODO in the future, be concurrent in these as well
    fetchArchives archives
    fetchRepos repos
  where
    s x = Endo (x:)
    run (Endo f) = f []
    (hackagesE, archivesE, reposE) = foldMap go pls
    hackages = run hackagesE
    archives = run archivesE
    repos = run reposE

    go (PLIHackage ident cfHash tree) = (s (toPir ident cfHash, Just tree), mempty, mempty)
    go (PLIArchive archive pm) = (mempty, s (archive, pm), mempty)
    go (PLIRepo repo pm) = (mempty, mempty, s (repo, pm))

    toPir (PackageIdentifier name ver) (BlobKey sha size) =
      PackageIdentifierRevision name ver (CFIHash sha (Just size))

-- | Unpack a given 'RawPackageLocationImmutable' into the given
-- directory. Does not generate any extra subdirectories.
--
-- @since 0.1.0.0
unpackPackageLocationRaw
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Path Abs Dir -- ^ unpack directory
  -> RawPackageLocationImmutable
  -> RIO env ()
unpackPackageLocationRaw fp loc = loadPackageRaw loc >>= unpackTree loc fp . packageTree

-- | Unpack a given 'PackageLocationImmutable' into the given
-- directory. Does not generate any extra subdirectories.
--
-- @since 0.1.0.0
unpackPackageLocation
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Path Abs Dir -- ^ unpack directory
  -> PackageLocationImmutable
  -> RIO env ()
unpackPackageLocation fp loc = loadPackage loc >>= unpackTree (toRawPLI loc) fp . packageTree

-- | Load the cabal file for the given 'PackageLocationImmutable'.
--
-- This function ignores all warnings.
--
-- @since 0.1.0.0
loadCabalFileImmutable
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageLocationImmutable
  -> RIO env GenericPackageDescription
loadCabalFileImmutable loc = withCache $ do
  logDebug $ "Parsing cabal file for " <> display loc
  bs <- loadCabalFileBytes loc
  (_warnings, gpd) <- rawParseGPD (Left $ toRawPLI loc) bs
  let pm =
        case loc of
          PLIHackage (PackageIdentifier name version) _cfHash mtree -> PackageMetadata
            { pmIdent = PackageIdentifier name version
            , pmTreeKey = mtree
            }
          PLIArchive _ pm' -> pm'
          PLIRepo _ pm' -> pm'
  let exc = MismatchedPackageMetadata (toRawPLI loc) (toRawPM pm) Nothing
        (gpdPackageIdentifier gpd)
      PackageIdentifier name ver = pmIdent pm
  maybe (throwIO exc) pure $ do
    guard $ name == gpdPackageName gpd
    guard $ ver == gpdVersion gpd
    pure gpd
  where
    withCache inner = do
      let rawLoc = toRawPLI loc
      ref <- view $ pantryConfigL.to pcParsedCabalFilesRawImmutable
      m0 <- readIORef ref
      case Map.lookup rawLoc m0 of
        Just x -> pure x
        Nothing -> do
          x <- inner
          atomicModifyIORef' ref $ \m -> (Map.insert rawLoc x m, x)

-- | Load the cabal file for the given 'RawPackageLocationImmutable'.
--
-- This function ignores all warnings.
--
-- Note that, for now, this will not allow support for hpack files in
-- these package locations. Instead, all @PackageLocationImmutable@s
-- will require a .cabal file. This may be relaxed in the future.
--
-- @since 0.1.0.0
loadCabalFileRawImmutable
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => RawPackageLocationImmutable
  -> RIO env GenericPackageDescription
loadCabalFileRawImmutable loc = withCache $ do
  logDebug $ "Parsing cabal file for " <> display loc
  bs <- loadRawCabalFileBytes loc
  (_warnings, gpd) <- rawParseGPD (Left loc) bs
  let rpm =
        case loc of
          RPLIHackage (PackageIdentifierRevision name version _cfi) mtree -> RawPackageMetadata
            { rpmName = Just name
            , rpmVersion = Just version
            , rpmTreeKey = mtree
            }
          RPLIArchive _ rpm' -> rpm'
          RPLIRepo _ rpm' -> rpm'
  let exc = MismatchedPackageMetadata loc rpm Nothing (gpdPackageIdentifier gpd)
  maybe (throwIO exc) pure $ do
    guard $ maybe True (== gpdPackageName gpd) (rpmName rpm)
    guard $ maybe True (== gpdVersion gpd) (rpmVersion rpm)
    pure gpd
  where
    withCache inner = do
      ref <- view $ pantryConfigL.to pcParsedCabalFilesRawImmutable
      m0 <- readIORef ref
      case Map.lookup loc m0 of
        Just x -> pure x
        Nothing -> do
          x <- inner
          atomicModifyIORef' ref $ \m -> (Map.insert loc x m, x)

-- | Same as 'loadCabalFileRawImmutable', but takes a
-- 'RawPackageLocation'. Never prints warnings, see 'loadCabalFilePath'
-- for that.
--
-- @since 0.1.0.0
loadCabalFileRaw
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => RawPackageLocation
  -> RIO env GenericPackageDescription
loadCabalFileRaw (RPLImmutable loc) = loadCabalFileRawImmutable loc
loadCabalFileRaw (RPLMutable rfp) = do
  (gpdio, _, _) <- loadCabalFilePath (resolvedAbsolute rfp)
  liftIO $ gpdio NoPrintWarnings

-- | Same as 'loadCabalFileImmutable', but takes a
-- 'PackageLocation'. Never prints warnings, see 'loadCabalFilePath'
-- for that.
--
-- @since 0.1.0.0
loadCabalFile
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageLocation
  -> RIO env GenericPackageDescription
loadCabalFile (PLImmutable loc) = loadCabalFileImmutable loc
loadCabalFile (PLMutable rfp) = do
  (gpdio, _, _) <- loadCabalFilePath (resolvedAbsolute rfp)
  liftIO $ gpdio NoPrintWarnings

-- | Parse the cabal file for the package inside the given
-- directory. Performs various sanity checks, such as the file name
-- being correct and having only a single cabal file.
--
-- @since 0.1.0.0
loadCabalFilePath
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Path Abs Dir -- ^ project directory, with a cabal file or hpack file
  -> RIO env
       ( PrintWarnings -> IO GenericPackageDescription
       , PackageName
       , Path Abs File
       )
loadCabalFilePath dir = do
  ref <- view $ pantryConfigL.to pcParsedCabalFilesMutable
  mcached <- Map.lookup dir <$> readIORef ref
  case mcached of
    Just triple -> pure triple
    Nothing -> do
      (name, cabalfp) <- findOrGenerateCabalFile dir
      gpdRef <- newIORef Nothing
      run <- askRunInIO
      let gpdio = run . getGPD cabalfp gpdRef
          triple = (gpdio, name, cabalfp)
      atomicModifyIORef' ref $ \m -> (Map.insert dir triple m, triple)
  where
    getGPD cabalfp gpdRef printWarnings = do
      mpair <- readIORef gpdRef
      (warnings0, gpd) <-
        case mpair of
          Just pair -> pure pair
          Nothing -> do
            bs <- liftIO $ B.readFile $ toFilePath cabalfp
            (warnings0, gpd) <- rawParseGPD (Right cabalfp) bs
            checkCabalFileName (gpdPackageName gpd) cabalfp
            pure (warnings0, gpd)
      warnings <-
        case printWarnings of
          YesPrintWarnings -> mapM_ (logWarn . toPretty cabalfp) warnings0 $> []
          NoPrintWarnings -> pure warnings0
      writeIORef gpdRef $ Just (warnings, gpd)
      pure gpd

    toPretty :: Path Abs File -> PWarning -> Utf8Builder
    toPretty src (PWarning _type pos msg) =
      "Cabal file warning in" <>
      fromString (toFilePath src) <> "@" <>
      fromString (showPos pos) <> ": " <>
      fromString msg

    -- | Check if the given name in the @Package@ matches the name of the .cabal file
    checkCabalFileName :: MonadThrow m => PackageName -> Path Abs File -> m ()
    checkCabalFileName name cabalfp = do
        -- Previously, we just use parsePackageNameFromFilePath. However, that can
        -- lead to confusing error messages. See:
        -- https://github.com/commercialhaskell/stack/issues/895
        let expected = T.unpack $ unSafeFilePath $ cabalFileName name
        when (expected /= toFilePath (filename cabalfp))
            $ throwM $ MismatchedCabalName cabalfp name

-- | Get the filename for the cabal file in the given directory.
--
-- If no .cabal file is present, or more than one is present, an exception is
-- thrown via 'throwM'.
--
-- If the directory contains a file named package.yaml, hpack is used to
-- generate a .cabal file from it.
--
-- @since 0.1.0.0
findOrGenerateCabalFile
    :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
    => Path Abs Dir -- ^ package directory
    -> RIO env (PackageName, Path Abs File)
findOrGenerateCabalFile pkgDir = do
    hpack pkgDir
    files <- filter (flip hasExtension "cabal" . toFilePath) . snd
         <$> listDir pkgDir
    -- If there are multiple files, ignore files that start with
    -- ".". On unixlike environments these are hidden, and this
    -- character is not valid in package names. The main goal is
    -- to ignore emacs lock files - see
    -- https://github.com/commercialhaskell/stack/issues/1897.
    let isHidden ('.':_) = True
        isHidden _ = False
    case filter (not . isHidden . toFilePath . filename) files of
        [] -> throwIO $ NoCabalFileFound pkgDir
        [x] -> maybe
          (throwIO $ InvalidCabalFilePath x)
          (\pn -> pure $ (pn, x)) $
            List.stripSuffix ".cabal" (toFilePath (filename x)) >>=
            parsePackageName
        _:_ -> throwIO $ MultipleCabalFilesFound pkgDir files
      where hasExtension fp x = FilePath.takeExtension fp == "." ++ x

-- | Generate .cabal file from package.yaml, if necessary.
hpack
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Path Abs Dir
  -> RIO env ()
hpack pkgDir = do
    packageConfigRelFile <- parseRelFile Hpack.packageConfig
    let hpackFile = pkgDir </> packageConfigRelFile
    exists <- liftIO $ doesFileExist hpackFile
    when exists $ do
        logDebug $ "Running hpack on " <> fromString (toFilePath hpackFile)

        he <- view $ pantryConfigL.to pcHpackExecutable
        case he of
            HpackBundled -> do
                r <- liftIO $ Hpack.hpackResult $ Hpack.setProgramName "stack" $ Hpack.setTarget (toFilePath hpackFile) Hpack.defaultOptions
                forM_ (Hpack.resultWarnings r) (logWarn . fromString)
                let cabalFile = fromString . Hpack.resultCabalFile $ r
                case Hpack.resultStatus r of
                    Hpack.Generated -> logDebug $ "hpack generated a modified version of " <> cabalFile
                    Hpack.OutputUnchanged -> logDebug $ "hpack output unchanged in " <> cabalFile
                    Hpack.AlreadyGeneratedByNewerHpack -> logWarn $
                        cabalFile <>
                        " was generated with a newer version of hpack,\n" <>
                        "please upgrade and try again."
                    Hpack.ExistingCabalFileWasModifiedManually -> logWarn $
                        cabalFile <>
                        " was modified manually. Ignoring " <>
                        fromString (toFilePath hpackFile) <>
                        " in favor of the cabal file.\nIf you want to use the " <>
                        fromString (toFilePath (filename hpackFile)) <>
                        " file instead of the cabal file,\n" <>
                        "then please delete the cabal file."
            HpackCommand command ->
                withWorkingDir (toFilePath pkgDir) $
                proc command [] runProcess_

-- | Get the 'PackageIdentifier' from a 'GenericPackageDescription'.
--
-- @since 0.1.0.0
gpdPackageIdentifier :: GenericPackageDescription -> PackageIdentifier
gpdPackageIdentifier = D.package . D.packageDescription

-- | Get the 'PackageName' from a 'GenericPackageDescription'.
--
-- @since 0.1.0.0
gpdPackageName :: GenericPackageDescription -> PackageName
gpdPackageName = pkgName . gpdPackageIdentifier

-- | Get the 'Version' from a 'GenericPackageDescription'.
--
-- @since 0.1.0.0
gpdVersion :: GenericPackageDescription -> Version
gpdVersion = pkgVersion . gpdPackageIdentifier

loadCabalFileBytes
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageLocationImmutable
  -> RIO env ByteString

-- Just ignore the mtree for this. Safe assumption: someone who filled
-- in the TreeKey also filled in the cabal file hash, and that's a
-- more efficient lookup mechanism.
loadCabalFileBytes (PLIHackage pident cfHash _mtree) = getHackageCabalFile (pirForHash pident cfHash)

loadCabalFileBytes pl = do
  package <- loadPackage pl
  let sfp = cabalFileName $ pkgName $ packageIdent package
  cabalBlobKey <- case (packageCabalEntry package) of
                       PCHpack pcHpack -> pure $ teBlob . phGenerated $ pcHpack
                       PCCabalFile (TreeEntry blobKey _) -> pure blobKey
  mbs <- withStorage $ loadBlob cabalBlobKey
  case mbs of
    Nothing -> do
      throwIO $ TreeReferencesMissingBlob (toRawPLI pl) sfp cabalBlobKey
    Just bs -> pure bs

-- FIXME: to be removed
loadRawCabalFileBytes
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => RawPackageLocationImmutable
  -> RIO env ByteString

-- Just ignore the mtree for this. Safe assumption: someone who filled
-- in the TreeKey also filled in the cabal file hash, and that's a
-- more efficient lookup mechanism.
loadRawCabalFileBytes (RPLIHackage pir _mtree) = getHackageCabalFile pir

loadRawCabalFileBytes pl = do
  package <- loadPackageRaw pl
  let sfp = cabalFileName $ pkgName $ packageIdent package
      TreeEntry cabalBlobKey _ft = case packageCabalEntry package of
                                     PCCabalFile cabalTE -> cabalTE
                                     PCHpack hpackCE -> phGenerated hpackCE
  mbs <- withStorage $ loadBlob cabalBlobKey
  case mbs of
    Nothing -> do
      throwIO $ TreeReferencesMissingBlob pl sfp cabalBlobKey
    Just bs -> pure bs

-- | Load a 'Package' from a 'PackageLocationImmutable'.
--
-- @since 0.1.0.0
loadPackage
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageLocationImmutable
  -> RIO env Package
loadPackage = loadPackageRaw . toRawPLI

-- | Load a 'Package' from a 'RawPackageLocationImmutable'.
--
-- Load the package either from the local DB, Casa, or as a last
-- resort, the third party (hackage, archive or repo).
--
-- @since 0.1.0.0
loadPackageRaw
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => RawPackageLocationImmutable
  -> RIO env Package
loadPackageRaw rpli =
  case getRawTreeKey rpli of
    Just treeKey' -> do
      mpackage <- tryLoadPackageRawViaDbOrCasa rpli treeKey'
      case mpackage of
        Nothing -> loadPackageRawViaThirdParty
        Just package -> pure package
    Nothing -> loadPackageRawViaThirdParty
  where
    loadPackageRawViaThirdParty = do
      logDebug ("Loading package from third-party: " <> display rpli)
      case rpli of
        RPLIHackage pir mtree -> htrPackage <$> getHackageTarball pir mtree
        RPLIArchive archive pm -> getArchivePackage rpli archive pm
        RPLIRepo repo rpm -> getRepo repo rpm

-- | Try to load a package via the database or Casa.
tryLoadPackageRawViaDbOrCasa ::
     (HasLogFunc env, HasPantryConfig env, HasProcessContext env)
  => RawPackageLocationImmutable
  -> TreeKey
  -> RIO env (Maybe Package)
tryLoadPackageRawViaDbOrCasa rpli treeKey' = do
  mviaDb <- tryLoadPackageRawViaLocalDb rpli treeKey'
  case mviaDb of
    Just package -> do
      logDebug ("Loaded package from Pantry: " <> display rpli)
      pure (Just package)
    Nothing -> do
      mviaCasa <- tryLoadPackageRawViaCasa rpli treeKey'
      case mviaCasa of
        Just package -> do
          logDebug ("Loaded package from Casa: " <> display rpli)
          pure (Just package)
        Nothing -> pure Nothing

-- | Maybe load the package from Casa.
tryLoadPackageRawViaCasa ::
     (HasLogFunc env, HasPantryConfig env, HasProcessContext env)
  => RawPackageLocationImmutable
  -> TreeKey
  -> RIO env (Maybe Package)
tryLoadPackageRawViaCasa rlpi treeKey' = do
  mtreePair <- casaLookupTree treeKey'
  case mtreePair of
    Nothing -> pure Nothing
    Just (treeKey'', _tree) -> do
      fetchTreeKeys [rlpi]
      mdb <- tryLoadPackageRawViaLocalDb rlpi treeKey''
      case mdb of
        Nothing -> do
          logWarn
            ("Did not find tree key in DB after pulling it from Casa: " <>
             display treeKey'' <>
             " (for " <>
             display rlpi <>
             ")")
          pure Nothing
        Just package -> pure (Just package)

-- | Maybe load the package from the local database.
tryLoadPackageRawViaLocalDb ::
     (HasLogFunc env, HasPantryConfig env, HasProcessContext env)
  => RawPackageLocationImmutable
  -> TreeKey
  -> RIO env (Maybe Package)
tryLoadPackageRawViaLocalDb rlpi treeKey' = do
  mtreeEntity <- withStorage (getTreeForKey treeKey')
  case mtreeEntity of
    Nothing -> pure Nothing
    Just treeId ->
      fmap Just (withStorage (loadPackageById rlpi (entityKey treeId)))

-- | Fill in optional fields in a 'PackageLocationImmutable' for more reproducible builds.
--
-- @since 0.1.0.0
completePackageLocation
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => RawPackageLocationImmutable
  -> RIO env PackageLocationImmutable
completePackageLocation (RPLIHackage (PackageIdentifierRevision n v (CFIHash sha (Just size))) (Just tk)) =
  pure $ PLIHackage (PackageIdentifier n v) (BlobKey sha size) tk
completePackageLocation (RPLIHackage pir0@(PackageIdentifierRevision name version cfi0) _) = do
  logDebug $ "Completing package location information from " <> display pir0
  (pir, cfKey) <-
    case cfi0 of
      CFIHash sha (Just size) -> pure (pir0, BlobKey sha size)
      _ -> do
        bs <- getHackageCabalFile pir0
        let size = FileSize (fromIntegral (B.length bs))
            sha = SHA256.hashBytes bs
            cfi = CFIHash sha (Just size)
            pir = PackageIdentifierRevision name version cfi
        logDebug $ "Added in cabal file hash: " <> display pir
        pure (pir, BlobKey sha size)
  treeKey' <- getHackageTarballKey pir
  pure $ PLIHackage (PackageIdentifier name version) cfKey treeKey'
completePackageLocation pl@(RPLIArchive archive rpm) = do
  mpackage <-
    case rpmTreeKey rpm of
      Just treeKey' -> tryLoadPackageRawViaDbOrCasa pl treeKey'
      Nothing -> pure Nothing
  case (,,) <$> raHash archive <*> raSize archive <*> mpackage of
    Just (sha256, fileSize, package) -> do
      let RawArchive loc _ _ subdir = archive
      pure $ PLIArchive (Archive loc sha256 fileSize subdir) (packagePM package)
    Nothing -> byThirdParty (isJust mpackage)
  where
    byThirdParty warnAboutMissingSizeSha = do
      (sha, size, package) <- getArchive pl archive rpm
      when warnAboutMissingSizeSha (warnWith sha size)
      -- (getArchive checks archive and package metadata)
      let RawArchive loc _ _ subdir = archive
      pure $ PLIArchive (Archive loc sha size subdir) (packagePM package)
    warnWith sha size =
      logWarn
        (mconcat
           [ "The package "
           , display pl
           , " is available from the local content-addressable storage database, \n"
           , "but we can't use it unless you specify the size and hash for this package.\n"
           , "Add the following to your package description:\n"
           , "\nsize: " <> display size
           , "\nsha256: " <> display sha
           ])
completePackageLocation pl@(RPLIRepo repo rpm) = do
  unless (isSHA1 (repoCommit repo)) $ throwIO $ CannotCompleteRepoNonSHA1 repo
  PLIRepo repo <$> completePM pl rpm
  where
    isSHA1 t = T.length t == 40 && T.all isHexDigit t

completePM
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => RawPackageLocationImmutable
  -> RawPackageMetadata
  -> RIO env PackageMetadata
completePM plOrig rpm@(RawPackageMetadata mn mv mtk)
  | Just n <- mn, Just v <- mv, Just tk <- mtk =
      pure $ PackageMetadata (PackageIdentifier n v) tk
  | otherwise = do
      pm <- packagePM <$> loadPackageRaw plOrig
      let isSame x (Just y) = x == y
          isSame _ _ = True

          allSame =
            isSame (pkgName $ pmIdent pm) (rpmName rpm) &&
            isSame (pkgVersion $ pmIdent pm) (rpmVersion rpm) &&
            isSame (pmTreeKey pm) (rpmTreeKey rpm)
      if allSame
        then pure pm
        else throwIO $ CompletePackageMetadataMismatch plOrig pm

packagePM :: Package -> PackageMetadata
packagePM package = PackageMetadata
  { pmIdent = packageIdent package
  , pmTreeKey = packageTreeKey package
  }

-- | Add in hashes to make a 'SnapshotLocation' reproducible.
--
-- @since 0.1.0.0
completeSnapshotLocation
  :: (HasPantryConfig env, HasLogFunc env)
  => RawSnapshotLocation
  -> RIO env SnapshotLocation
completeSnapshotLocation (RSLCompiler c) = pure $ SLCompiler c
completeSnapshotLocation (RSLFilePath f) = pure $ SLFilePath f
completeSnapshotLocation (RSLUrl url (Just blobKey)) = pure $ SLUrl url blobKey
completeSnapshotLocation (RSLUrl url Nothing) = do
  bs <- loadFromURL url Nothing
  pure $ SLUrl url (bsToBlobKey bs)

-- | Fill in optional fields in a 'SnapshotLayer' for more reproducible builds.
--
-- @since 0.1.0.0
completeSnapshotLayer
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => RawSnapshotLayer
  -> RIO env SnapshotLayer
completeSnapshotLayer rsnapshot = do
  parent' <- completeSnapshotLocation $ rslParent rsnapshot
  pls <- traverseConcurrently completePackageLocation $ rslLocations rsnapshot
  pure SnapshotLayer
    { slParent = parent'
    , slLocations = pls
    , slCompiler= rslCompiler rsnapshot
    , slDropPackages = rslDropPackages rsnapshot
    , slFlags = rslFlags rsnapshot
    , slHidden = rslHidden rsnapshot
    , slGhcOptions = rslGhcOptions rsnapshot
    , slPublishTime = rslPublishTime rsnapshot
    }

traverseConcurrently_
  :: (Foldable f, HasPantryConfig env)
  => (a -> RIO env ()) -- ^ action to perform
  -> f a -- ^ input values
  -> RIO env ()
traverseConcurrently_ f t0 = do
  cnt <- view $ pantryConfigL.to pcConnectionCount
  traverseConcurrentlyWith_ cnt f t0

traverseConcurrentlyWith_
  :: (MonadUnliftIO m, Foldable f)
  => Int -- ^ concurrent workers
  -> (a -> m ()) -- ^ action to perform
  -> f a -- ^ input values
  -> m ()
traverseConcurrentlyWith_ count f t0 = do
  queue <- newTVarIO $ toList t0

  replicateConcurrently_ count $
    fix $ \loop -> join $ atomically $ do
      toProcess <- readTVar queue
      case toProcess of
        [] -> pure (pure ())
        (x:rest) -> do
          writeTVar queue rest
          pure $ do
            f x
            loop

traverseConcurrently
  :: (HasPantryConfig env, Traversable t)
  => (a -> RIO env b) -- ^ action to perform
  -> t a -- ^ input values
  -> RIO env (t b)
traverseConcurrently f t0 = do
  cnt <- view $ pantryConfigL.to pcConnectionCount
  traverseConcurrentlyWith cnt f t0

-- | Like 'traverse', but does things on
-- up to N separate threads at once.
traverseConcurrentlyWith
  :: (MonadUnliftIO m, Traversable t)
  => Int -- ^ concurrent workers
  -> (a -> m b) -- ^ action to perform
  -> t a -- ^ input values
  -> m (t b)
traverseConcurrentlyWith count f t0 = do
  (queue, t1) <- atomically $ do
    queueDList <- newTVar id
    t1 <- for t0 $ \x -> do
      res <- newEmptyTMVar
      modifyTVar queueDList (. ((x, res):))
      pure $ atomically $ takeTMVar res
    dlist <- readTVar queueDList
    queue <- newTVar $ dlist []
    pure (queue, t1)

  replicateConcurrently_ count $
    fix $ \loop -> join $ atomically $ do
      toProcess <- readTVar queue
      case toProcess of
        [] -> pure (pure ())
        ((x, res):rest) -> do
          writeTVar queue rest
          pure $ do
            y <- f x
            atomically $ putTMVar res y
            loop
  sequence t1

-- | Parse a 'RawSnapshot' (all layers) from a 'RawSnapshotLocation'.
--
-- @since 0.1.0.0
loadSnapshotRaw
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => RawSnapshotLocation
  -> RIO env RawSnapshot
loadSnapshotRaw loc = do
  eres <- loadRawSnapshotLayer loc
  case eres of
    Left wc ->
      pure RawSnapshot
        { rsCompiler = wc
        , rsPackages = mempty
        , rsDrop = mempty
        }
    Right (rsl, _) -> do
      snap0 <- loadSnapshotRaw $ rslParent rsl
      (packages, unused) <-
        addPackagesToSnapshot
          (display loc)
          (rslLocations rsl)
          AddPackagesConfig
            { apcDrop = rslDropPackages rsl
            , apcFlags = rslFlags rsl
            , apcHiddens = rslHidden rsl
            , apcGhcOptions = rslGhcOptions rsl
            }
          (rsPackages snap0)
      warnUnusedAddPackagesConfig (display loc) unused
      pure RawSnapshot
        { rsCompiler = fromMaybe (rsCompiler snap0) (rslCompiler rsl)
        , rsPackages = packages
        , rsDrop = apcDrop unused
        }

-- | Parse a 'RawSnapshot' (all layers) from a 'SnapshotLocation'.
--
-- @since 0.1.0.0
loadSnapshot
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => SnapshotLocation
  -> RIO env RawSnapshot
loadSnapshot loc = do
  eres <- loadSnapshotLayer loc
  case eres of
    Left wc ->
      pure RawSnapshot
        { rsCompiler = wc
        , rsPackages = mempty
        , rsDrop = mempty
        }
    Right rsl -> do
      snap0 <- loadSnapshotRaw $ rslParent rsl
      (packages, unused) <-
        addPackagesToSnapshot
          (display loc)
          (rslLocations rsl)
          AddPackagesConfig
            { apcDrop = rslDropPackages rsl
            , apcFlags = rslFlags rsl
            , apcHiddens = rslHidden rsl
            , apcGhcOptions = rslGhcOptions rsl
            }
          (rsPackages snap0)
      warnUnusedAddPackagesConfig (display loc) unused
      pure RawSnapshot
        { rsCompiler = fromMaybe (rsCompiler snap0) (rslCompiler rsl)
        , rsPackages = packages
        , rsDrop = apcDrop unused
        }

-- | A completed package location, including the original raw and completed information.
--
-- @since 0.1.0.0
data CompletedPLI = CompletedPLI !RawPackageLocationImmutable !PackageLocationImmutable

-- | A completed snapshot location, including the original raw and completed information.
--
-- @since 0.1.0.0
data CompletedSL = CompletedSL !RawSnapshotLocation !SnapshotLocation

-- | Parse a 'Snapshot' (all layers) from a 'SnapshotLocation' noting
-- any incomplete package locations
--
-- @since 0.1.0.0
loadAndCompleteSnapshot
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => SnapshotLocation
  -> Map RawSnapshotLocation SnapshotLocation -- ^ Cached snapshot locations from lock file
  -> Map RawPackageLocationImmutable PackageLocationImmutable -- ^ Cached locations from lock file
  -> RIO env (Snapshot, [CompletedSL], [CompletedPLI])
loadAndCompleteSnapshot loc cachedSL cachedPL =
  loadAndCompleteSnapshotRaw (toRawSL loc) cachedSL cachedPL

-- | Parse a 'Snapshot' (all layers) from a 'RawSnapshotLocation' completing
-- any incomplete package locations
--
-- @since 0.1.0.0
loadAndCompleteSnapshotRaw
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => RawSnapshotLocation
  -> Map RawSnapshotLocation SnapshotLocation -- ^ Cached snapshot locations from lock file
  -> Map RawPackageLocationImmutable PackageLocationImmutable -- ^ Cached locations from lock file
  -> RIO env (Snapshot, [CompletedSL], [CompletedPLI])
loadAndCompleteSnapshotRaw rawLoc cacheSL cachePL = do
  eres <- case Map.lookup rawLoc cacheSL of
    Just loc -> right (\rsl -> (rsl, (CompletedSL rawLoc loc))) <$> loadSnapshotLayer loc
    Nothing -> loadRawSnapshotLayer rawLoc
  case eres of
    Left wc ->
      let snapshot = Snapshot
            { snapshotCompiler = wc
            , snapshotPackages = mempty
            , snapshotDrop = mempty
            }
      in pure (snapshot, [CompletedSL (RSLCompiler wc) (SLCompiler wc)], [])
    Right (rsl, sloc) -> do
      (snap0, slocs, completed0) <- loadAndCompleteSnapshotRaw (rslParent rsl) cacheSL cachePL
      (packages, completed, unused) <-
        addAndCompletePackagesToSnapshot
          rawLoc
          cachePL
          (rslLocations rsl)
          AddPackagesConfig
            { apcDrop = rslDropPackages rsl
            , apcFlags = rslFlags rsl
            , apcHiddens = rslHidden rsl
            , apcGhcOptions = rslGhcOptions rsl
            }
          (snapshotPackages snap0)
      warnUnusedAddPackagesConfig (display rawLoc) unused
      let snapshot = Snapshot
            { snapshotCompiler = fromMaybe (snapshotCompiler snap0) (rslCompiler rsl)
            , snapshotPackages = packages
            , snapshotDrop = apcDrop unused
            }
      return (snapshot, sloc : slocs,completed0 ++ completed)

data SingleOrNot a
  = Single !a
  | Multiple !a !a !([a] -> [a])
instance Semigroup (SingleOrNot a) where
  Single a <> Single b = Multiple a b id
  Single a <> Multiple b c d = Multiple a b ((c:) . d)
  Multiple a b c <> Single d = Multiple a b (c . (d:))
  Multiple a b c <> Multiple d e f =
    Multiple a b (c . (d:) . (e:) . f)

sonToEither :: (k, SingleOrNot a) -> Either (k, a) (k, [a])
sonToEither (k, Single a) = Left (k, a)
sonToEither (k, Multiple a b c) = Right (k, (a : b : c []))

-- | Package settings to be passed to 'addPackagesToSnapshot'.
--
-- @since 0.1.0.0
data AddPackagesConfig = AddPackagesConfig
  { apcDrop :: !(Set PackageName)
  , apcFlags :: !(Map PackageName (Map FlagName Bool))
  , apcHiddens :: !(Map PackageName Bool)
  , apcGhcOptions :: !(Map PackageName [Text])
  }

-- | Does not warn about drops, those are allowed in order to ignore global
-- packages.
warnUnusedAddPackagesConfig
  :: HasLogFunc env
  => Utf8Builder -- ^ source
  -> AddPackagesConfig
  -> RIO env ()
warnUnusedAddPackagesConfig source (AddPackagesConfig _drops flags hiddens options) = do
  unless (null ls) $ do
    logWarn $ "Some warnings discovered when adding packages to snapshot (" <> source <> ")"
    traverse_ logWarn ls
  where
    ls = concat [flags', hiddens', options']

    flags' =
      map
        (\pn ->
          "Setting flags for non-existent package: " <>
          fromString (packageNameString pn))
        (Map.keys flags)

    hiddens' =
      map
        (\pn ->
          "Hiding non-existent package: " <>
          fromString (packageNameString pn))
        (Map.keys hiddens)

    options' =
      map
        (\pn ->
          "Setting options for non-existent package: " <>
          fromString (packageNameString pn))
        (Map.keys options)

-- | Add more packages to a snapshot
--
-- Note that any settings on a parent flag which is being replaced will be
-- ignored. For example, if package @foo@ is in the parent and has flag @bar@
-- set, and @foo@ also appears in new packages, then @bar@ will no longer be
-- set.
--
-- Returns any of the 'AddPackagesConfig' values not used.
--
-- @since 0.1.0.0
addPackagesToSnapshot
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Utf8Builder
  -- ^ Text description of where these new packages are coming from, for error
  -- messages only
  -> [RawPackageLocationImmutable] -- ^ new packages
  -> AddPackagesConfig
  -> Map PackageName RawSnapshotPackage -- ^ packages from parent
  -> RIO env (Map PackageName RawSnapshotPackage, AddPackagesConfig)
addPackagesToSnapshot source newPackages (AddPackagesConfig drops flags hiddens options) old = do
  new' <- for newPackages $ \loc -> do
    name <- getPackageLocationName loc
    pure (name, RawSnapshotPackage
      { rspLocation = loc
      , rspFlags = Map.findWithDefault mempty name flags
      , rspHidden = Map.findWithDefault False name hiddens
      , rspGhcOptions = Map.findWithDefault [] name options
      })
  let (newSingles, newMultiples)
        = partitionEithers
        $ map sonToEither
        $ Map.toList
        $ Map.fromListWith (<>)
        $ map (second Single) new'
  unless (null $ newMultiples) $ throwIO $
    DuplicatePackageNames source $ map (second (map rspLocation)) newMultiples
  let new = Map.fromList newSingles
      allPackages0 = new `Map.union` (old `Map.difference` Map.fromSet (const ()) drops)
      allPackages = flip Map.mapWithKey allPackages0 $ \name rsp ->
        rsp
          { rspFlags = Map.findWithDefault (rspFlags rsp) name flags
          , rspHidden = Map.findWithDefault (rspHidden rsp) name hiddens
          , rspGhcOptions = Map.findWithDefault (rspGhcOptions rsp) name options
          }

      unused = AddPackagesConfig
        (drops `Set.difference` Map.keysSet old)
        (flags `Map.difference` allPackages)
        (hiddens `Map.difference` allPackages)
        (options `Map.difference` allPackages)

  pure (allPackages, unused)

cachedSnapshotCompletePackageLocation :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Map RawPackageLocationImmutable PackageLocationImmutable
  -> RawPackageLocationImmutable
  -> RIO env PackageLocationImmutable
cachedSnapshotCompletePackageLocation cachePackages rpli = do
  let xs = Map.lookup rpli cachePackages
  case xs of
    Nothing -> completePackageLocation rpli
    Just x -> pure x

-- | Add more packages to a snapshot completing their locations if needed
--
-- Note that any settings on a parent flag which is being replaced will be
-- ignored. For example, if package @foo@ is in the parent and has flag @bar@
-- set, and @foo@ also appears in new packages, then @bar@ will no longer be
-- set.
--
-- Returns any of the 'AddPackagesConfig' values not used and also all
-- non-trivial package location completions.
--
-- @since 0.1.0.0
addAndCompletePackagesToSnapshot
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => RawSnapshotLocation
  -- ^ Text description of where these new packages are coming from, for error
  -- messages only
  -> Map RawPackageLocationImmutable PackageLocationImmutable -- ^ Cached data from snapshot lock file
  -> [RawPackageLocationImmutable] -- ^ new packages
  -> AddPackagesConfig
  -> Map PackageName SnapshotPackage -- ^ packages from parent
  -> RIO env (Map PackageName SnapshotPackage, [CompletedPLI], AddPackagesConfig)
addAndCompletePackagesToSnapshot loc cachedPL newPackages (AddPackagesConfig drops flags hiddens options) old = do
  let source = display loc
      addPackage :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
                 => ([(PackageName, SnapshotPackage)],[CompletedPLI])
                 -> RawPackageLocationImmutable
                 -> RIO env ([(PackageName, SnapshotPackage)], [CompletedPLI])
      addPackage (ps, completed) rawLoc = do
        complLoc <- cachedSnapshotCompletePackageLocation cachedPL rawLoc
        let PackageIdentifier name _ = packageLocationIdent complLoc
            p = (name, SnapshotPackage
              { spLocation = complLoc
              , spFlags = Map.findWithDefault mempty name flags
              , spHidden = Map.findWithDefault False name hiddens
              , spGhcOptions = Map.findWithDefault [] name options
              })
            completed' = if toRawPLI complLoc == rawLoc
                         then completed
                         else CompletedPLI rawLoc complLoc:completed
        pure (p:ps, completed')
  (revNew, revCompleted) <- foldM addPackage ([], []) newPackages
  let (newSingles, newMultiples)
        = partitionEithers
        $ map sonToEither
        $ Map.toList
        $ Map.fromListWith (<>)
        $ map (second Single) (reverse revNew)
  unless (null $ newMultiples) $ throwIO $
    DuplicatePackageNames source $ map (second (map (toRawPLI . spLocation))) newMultiples
  let new = Map.fromList newSingles
      allPackages0 = new `Map.union` (old `Map.difference` Map.fromSet (const ()) drops)
      allPackages = flip Map.mapWithKey allPackages0 $ \name sp ->
        sp
          { spFlags = Map.findWithDefault (spFlags sp) name flags
          , spHidden = Map.findWithDefault (spHidden sp) name hiddens
          , spGhcOptions = Map.findWithDefault (spGhcOptions sp) name options
          }

      unused = AddPackagesConfig
        (drops `Set.difference` Map.keysSet old)
        (flags `Map.difference` allPackages)
        (hiddens `Map.difference` allPackages)
        (options `Map.difference` allPackages)

  pure (allPackages, reverse revCompleted, unused)

-- | Parse a 'SnapshotLayer' value from a 'SnapshotLocation'.
--
-- Returns a 'Left' value if provided an 'SLCompiler'
-- constructor. Otherwise, returns a 'Right' value providing both the
-- 'Snapshot' and a hash of the input configuration file.
--
-- @since 0.1.0.0
loadRawSnapshotLayer
  :: (HasPantryConfig env, HasLogFunc env)
  => RawSnapshotLocation
  -> RIO env (Either WantedCompiler (RawSnapshotLayer, CompletedSL))
loadRawSnapshotLayer (RSLCompiler compiler) = pure $ Left compiler
loadRawSnapshotLayer rsl@(RSLUrl url blob) =
  handleAny (throwIO . InvalidSnapshot rsl) $ do
    bs <- loadFromURL url blob
    value <- Yaml.decodeThrow bs
    snapshot <- warningsParserHelperRaw rsl value Nothing
    pure $ Right (snapshot, (CompletedSL rsl (SLUrl url (bsToBlobKey bs))))
loadRawSnapshotLayer rsl@(RSLFilePath fp) =
  handleAny (throwIO . InvalidSnapshot rsl) $ do
    value <- Yaml.decodeFileThrow $ toFilePath $ resolvedAbsolute fp
    snapshot <- warningsParserHelperRaw rsl value $ Just $ parent $ resolvedAbsolute fp
    pure $ Right (snapshot, CompletedSL rsl (SLFilePath fp))

-- | Parse a 'SnapshotLayer' value from a 'SnapshotLocation'.
--
-- Returns a 'Left' value if provided an 'SLCompiler'
-- constructor. Otherwise, returns a 'Right' value providing both the
-- 'Snapshot' and a hash of the input configuration file.
--
-- @since 0.1.0.0
loadSnapshotLayer
  :: (HasPantryConfig env, HasLogFunc env)
  => SnapshotLocation
  -> RIO env (Either WantedCompiler RawSnapshotLayer)
loadSnapshotLayer (SLCompiler compiler) = pure $ Left compiler
loadSnapshotLayer sl@(SLUrl url blob) =
  handleAny (throwIO . InvalidSnapshot (toRawSL sl)) $ do
    bs <- loadFromURL url (Just blob)
    value <- Yaml.decodeThrow bs
    snapshot <- warningsParserHelper sl value Nothing
    pure $ Right snapshot
loadSnapshotLayer sl@(SLFilePath fp) =
  handleAny (throwIO . InvalidSnapshot (toRawSL sl)) $ do
    value <- Yaml.decodeFileThrow $ toFilePath $ resolvedAbsolute fp
    snapshot <- warningsParserHelper sl value $ Just $ parent $ resolvedAbsolute fp
    pure $ Right snapshot

loadFromURL
  :: (HasPantryConfig env, HasLogFunc env)
  => Text -- ^ url
  -> Maybe BlobKey
  -> RIO env ByteString
loadFromURL url Nothing = do
  mcached <- withStorage $ loadURLBlob url
  case mcached of
    Just bs -> return bs
    Nothing -> loadWithCheck url Nothing
loadFromURL url (Just bkey) = do
  mcached <- withStorage $ loadBlob bkey
  case mcached of
    Just bs -> do
      logDebug "Loaded snapshot from Pantry database."
      return bs
    Nothing -> loadUrlViaCasaOrWithCheck url bkey

loadUrlViaCasaOrWithCheck
  :: (HasPantryConfig env, HasLogFunc env)
  => Text -- ^ url
  -> BlobKey
  -> RIO env ByteString
loadUrlViaCasaOrWithCheck url blobKey = do
  mblobFromCasa <- casaLookupKey blobKey
  case mblobFromCasa of
    Just blob -> do
      logDebug
        ("Loaded snapshot from Casa (" <> display blobKey <> ") for URL: " <>
         display url)
      pure blob
    Nothing -> loadWithCheck url (Just blobKey)

loadWithCheck
  :: (HasPantryConfig env, HasLogFunc env)
  => Text -- ^ url
  -> Maybe BlobKey
  -> RIO env ByteString
loadWithCheck url mblobkey = do
  let (msha, msize) =
        case mblobkey of
          Nothing -> (Nothing, Nothing)
          Just (BlobKey sha size) -> (Just sha, Just size)
  (_, _, bss) <- httpSinkChecked url msha msize sinkList
  let bs = B.concat bss
  withStorage $ storeURLBlob url bs
  logDebug ("Loaded snapshot from third party: " <> display url)
  return bs

warningsParserHelperRaw
  :: HasLogFunc env
  => RawSnapshotLocation
  -> Value
  -> Maybe (Path Abs Dir)
  -> RIO env RawSnapshotLayer
warningsParserHelperRaw rsl val mdir =
  case parseEither Yaml.parseJSON val of
    Left e -> throwIO $ Couldn'tParseSnapshot rsl e
    Right (WithJSONWarnings x ws) -> do
      unless (null ws) $ do
        logWarn $ "Warnings when parsing snapshot " <> display rsl
        for_ ws $ logWarn . display
      resolvePaths mdir x

warningsParserHelper
  :: HasLogFunc env
  => SnapshotLocation
  -> Value
  -> Maybe (Path Abs Dir)
  -> RIO env RawSnapshotLayer
warningsParserHelper sl val mdir =
  case parseEither Yaml.parseJSON val of
    Left e -> throwIO $ Couldn'tParseSnapshot (toRawSL sl) e
    Right (WithJSONWarnings x ws) -> do
      unless (null ws) $ do
        logWarn $ "Warnings when parsing snapshot " <> display sl
        for_ ws $ logWarn . display
      resolvePaths mdir x

-- | Get the 'PackageName' of the package at the given location.
--
-- @since 0.1.0.0
getPackageLocationName
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => RawPackageLocationImmutable
  -> RIO env PackageName
getPackageLocationName = fmap pkgName . getRawPackageLocationIdent

-- | Get the 'PackageIdentifier' of the package at the given location.
--
-- @since 0.1.0.0
packageLocationIdent
  :: PackageLocationImmutable
  -> PackageIdentifier
packageLocationIdent (PLIHackage ident _ _) = ident
packageLocationIdent (PLIRepo _ pm) = pmIdent pm
packageLocationIdent (PLIArchive _ pm) = pmIdent pm

-- | Get version of the package at the given location.
--
-- @since 0.1.0.0
packageLocationVersion
  :: PackageLocationImmutable
  -> Version
packageLocationVersion (PLIHackage pident _ _) = pkgVersion pident
packageLocationVersion (PLIRepo _ pm) = pkgVersion (pmIdent pm)
packageLocationVersion (PLIArchive _ pm) = pkgVersion (pmIdent pm)

-- | Get the 'PackageIdentifier' of the package at the given location.
--
-- @since 0.1.0.0
getRawPackageLocationIdent
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => RawPackageLocationImmutable
  -> RIO env PackageIdentifier
getRawPackageLocationIdent (RPLIHackage (PackageIdentifierRevision name version _) _) = pure $ PackageIdentifier name version
getRawPackageLocationIdent (RPLIRepo _ RawPackageMetadata { rpmName = Just name, rpmVersion = Just version }) = pure $ PackageIdentifier name version
getRawPackageLocationIdent (RPLIArchive _ RawPackageMetadata { rpmName = Just name, rpmVersion = Just version }) = pure $ PackageIdentifier name version
getRawPackageLocationIdent rpli = packageIdent <$> loadPackageRaw rpli

-- | Get the 'TreeKey' of the package at the given location.
--
-- @since 0.1.0.0
getRawPackageLocationTreeKey
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => RawPackageLocationImmutable
  -> RIO env TreeKey
getRawPackageLocationTreeKey pl =
  case getRawTreeKey pl of
    Just treeKey' -> pure treeKey'
    Nothing ->
      case pl of
        RPLIHackage pir _ -> getHackageTarballKey pir
        RPLIArchive archive pm -> getArchiveKey pl archive pm
        RPLIRepo repo pm -> getRepoKey repo pm

-- | Get the 'TreeKey' of the package at the given location.
--
-- @since 0.1.0.0
getPackageLocationTreeKey
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageLocationImmutable
  -> RIO env TreeKey
getPackageLocationTreeKey pl = pure $ getTreeKey pl

getRawTreeKey :: RawPackageLocationImmutable -> Maybe TreeKey
getRawTreeKey (RPLIHackage _ mtree) = mtree
getRawTreeKey (RPLIArchive _ rpm) = rpmTreeKey rpm
getRawTreeKey (RPLIRepo _ rpm) = rpmTreeKey rpm

getTreeKey :: PackageLocationImmutable -> TreeKey
getTreeKey (PLIHackage _ _ tree) = tree
getTreeKey (PLIArchive _ pm) = pmTreeKey pm
getTreeKey (PLIRepo _ pm) = pmTreeKey pm

-- | Convenient data type that allows you to work with pantry more
-- easily than using 'withPantryConfig' directly. Uses basically sane
-- settings, like sharing a pantry directory with Stack.
--
-- You can use 'runPantryApp' to use this.
--
-- @since 0.1.0.0
data PantryApp = PantryApp
  { paSimpleApp :: !SimpleApp
  , paPantryConfig :: !PantryConfig
  , paUseColor :: !Bool
  , paTermWidth :: !Int
  , paStylesUpdate :: !StylesUpdate
  }

simpleAppL :: Lens' PantryApp SimpleApp
simpleAppL = lens paSimpleApp (\x y -> x { paSimpleApp = y })

-- | Lens to view or modify the 'HpackExecutable' of a 'PantryConfig'
--
-- @since 0.1.0.0
hpackExecutableL :: Lens' PantryConfig HpackExecutable
hpackExecutableL k pconfig = fmap (\hpExe -> pconfig { pcHpackExecutable = hpExe }) (k (pcHpackExecutable pconfig))

instance HasLogFunc PantryApp where
  logFuncL = simpleAppL.logFuncL
instance HasPantryConfig PantryApp where
  pantryConfigL = lens paPantryConfig (\x y -> x { paPantryConfig = y })
instance HasProcessContext PantryApp where
  processContextL = simpleAppL.processContextL
instance HasStylesUpdate PantryApp where
  stylesUpdateL = lens paStylesUpdate (\x y -> x { paStylesUpdate = y })
instance HasTerm PantryApp where
  useColorL = lens paUseColor (\x y -> x { paUseColor = y })
  termWidthL = lens paTermWidth  (\x y -> x { paTermWidth = y })

-- | Run some code against pantry using basic sane settings.
--
-- For testing, see 'runPantryAppClean'.
--
-- @since 0.1.0.0
runPantryApp :: MonadIO m => RIO PantryApp a -> m a
runPantryApp = runPantryAppWith 8 defaultCasaRepoPrefix defaultCasaMaxPerRequest

-- | Run some code against pantry using basic sane settings.
--
-- For testing, see 'runPantryAppClean'.
--
-- @since 0.1.1.1
runPantryAppWith :: MonadIO m => Int -> CasaRepoPrefix -> Int -> RIO PantryApp a -> m a
runPantryAppWith maxConnCount casaRepoPrefix casaMaxPerRequest f = runSimpleApp $ do
  sa <- ask
  stack <- getAppUserDataDirectory "stack"
  root <- parseAbsDir $ stack FilePath.</> "pantry"
  withPantryConfig
    root
    defaultHackageSecurityConfig
    HpackBundled
    maxConnCount
    casaRepoPrefix
    casaMaxPerRequest
    $ \pc ->
      runRIO
        PantryApp
          { paSimpleApp = sa
          , paPantryConfig = pc
          , paTermWidth = 100
          , paUseColor = True
          , paStylesUpdate = mempty
          }
        f

-- | Like 'runPantryApp', but uses an empty pantry directory instead
-- of sharing with Stack. Useful for testing.
--
-- @since 0.1.0.0
runPantryAppClean :: MonadIO m => RIO PantryApp a -> m a
runPantryAppClean f = liftIO $ withSystemTempDirectory "pantry-clean" $ \dir -> runSimpleApp $ do
  sa <- ask
  root <- resolveDir' dir
  withPantryConfig
    root
    defaultHackageSecurityConfig
    HpackBundled
    8
    defaultCasaRepoPrefix
    defaultCasaMaxPerRequest
    $ \pc ->
      runRIO
        PantryApp
          { paSimpleApp = sa
          , paPantryConfig = pc
          , paTermWidth = 100
          , paUseColor = True
          , paStylesUpdate = mempty
          }
        f

-- | Load the global hints from Github.
--
-- @since 0.1.0.0
loadGlobalHints
  :: (HasTerm env, HasPantryConfig env)
  => WantedCompiler
  -> RIO env (Maybe (Map PackageName Version))
loadGlobalHints wc =
    inner False
  where
    inner alreadyDownloaded = do
      dest <- getGlobalHintsFile
      req <- parseRequest "https://raw.githubusercontent.com/fpco/stackage-content/master/stack/global-hints.yaml"
      downloaded <- download req dest
      eres <- tryAny (inner2 dest)
      mres <-
        case eres of
          Left e -> Nothing <$ logError ("Error when parsing global hints: " <> displayShow e)
          Right x -> pure x
      case mres of
        Nothing | not alreadyDownloaded && not downloaded -> do
          logInfo $
            "Could not find local global hints for " <>
            RIO.display wc <>
            ", forcing a redownload"
          x <- redownload req dest
          if x
            then inner True
            else do
              logInfo "Redownload didn't happen"
              pure Nothing
        _ -> pure mres

    inner2 dest
           = liftIO
           $ Map.lookup wc . fmap (fmap unCabalString . unCabalStringMap)
         <$> Yaml.decodeFileThrow (toFilePath dest)

-- | Partition a map of global packages with its versions into a Set of
-- replaced packages and its dependencies and a map of remaining (untouched) packages.
--
-- @since 0.1.0.0
partitionReplacedDependencies ::
       Ord id
    => Map PackageName a -- ^ global packages
    -> (a -> PackageName) -- ^ package name getter
    -> (a -> id) -- ^ returns unique package id used for dependency pruning
    -> (a -> [id]) -- ^ returns unique package ids of direct package dependencies
    -> Set PackageName -- ^ overrides which global dependencies should get pruned
    -> (Map PackageName [PackageName], Map PackageName a)
partitionReplacedDependencies globals getName getId getDeps overrides =
  flip execState (replaced, mempty) $
    for (Map.toList globals) $ prunePackageWithDeps globals' getName getDeps
  where
    globals' = Map.fromList $ map (getId &&& id) (Map.elems globals)
    replaced = Map.map (const []) $ Map.restrictKeys globals overrides

prunePackageWithDeps ::
       Ord id
    => Map id a
    -> (a -> PackageName)
    -> (a -> [id])
    -> (PackageName, a)
    -> State (Map PackageName [PackageName], Map PackageName a) Bool
prunePackageWithDeps pkgs getName getDeps (pname, a)  = do
  (pruned, kept) <- get
  if Map.member pname pruned
  then return True
  else if Map.member pname kept
    then return False
    else do
      let deps = Map.elems $ Map.restrictKeys pkgs (Set.fromList $ getDeps a)
      prunedDeps <- forMaybeM deps $ \dep -> do
        let depName = getName dep
        isPruned <- prunePackageWithDeps pkgs getName getDeps (depName, dep)
        pure $ if isPruned then Just depName else Nothing
      if null prunedDeps
      then do
        modify' $ second (Map.insert pname a)
      else do
        modify' $ first (Map.insert pname prunedDeps)
      return $ not (null prunedDeps)

-- | Use a snapshot cache, which caches which modules are in which
-- packages in a given snapshot. This is mostly intended for usage by
-- Stack.
--
-- @since 0.1.0.0
withSnapshotCache
  :: (HasPantryConfig env, HasLogFunc env)
  => SnapshotCacheHash
  -> RIO env (Map PackageName (Set ModuleName))
  -> ((ModuleName -> RIO env [PackageName]) -> RIO env a)
  -> RIO env a
withSnapshotCache hash getModuleMapping f = do
  mres <- withStorage $ getSnapshotCacheByHash hash
  cacheId <- case mres of
    Nothing -> do
      logWarn "Populating snapshot module name cache"
      packageModules <- getModuleMapping
      withStorage $ do
        scId <- getSnapshotCacheId hash
        storeSnapshotModuleCache scId packageModules
        return scId
    Just scId -> pure scId
  f $ withStorage . loadExposedModulePackages cacheId

-- | Add an s to the builder if n!=1.
plural :: Int -> Utf8Builder -> Utf8Builder
plural n text =
  display n <> " " <> text <>
  (if n == 1
     then ""
     else "s")
