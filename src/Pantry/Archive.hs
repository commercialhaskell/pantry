{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Logic for loading up trees from HTTPS archives.
module Pantry.Archive
  ( getArchivePackage
  , getArchive
  , getArchiveKey
  , fetchArchivesRaw
  , fetchArchives
  , findCabalOrHpackFile
  ) where

import qualified Codec.Archive.Zip as Zip
import           Conduit
                   ( ConduitT, (.|), runConduit, sinkHandle, sinkList
                   , sourceHandle, sourceLazy, withSourceFile
                   )
import           Data.Bits ( (.&.), shiftR )
import qualified Data.Conduit.Tar as Tar
import           Data.Conduit.Zlib ( ungzip )
import qualified Data.Digest.CRC32 as CRC32
import           Distribution.PackageDescription ( package, packageDescription )
import qualified Hpack.Config as Hpack
import           Pantry.HPack ( hpackVersion )
import           Pantry.HTTP ( httpSinkChecked )
import           Pantry.Internal ( makeTarRelative, normalizeParents )
import qualified Pantry.SHA256 as SHA256
import           Pantry.Storage
                   ( BlobId, CachedTree (..), TreeId, hpackToCabal
                   , loadArchiveCache, loadBlob, loadCabalBlobKey
                   , loadCachedTree, loadPackageById, storeArchiveCache
                   , storeBlob, storeHPack, storeTree, unCachedTree, withStorage
                   )
import           Pantry.Tree ( rawParseGPD )
import           Pantry.Types
                   ( Archive, ArchiveLocation (..), BlobKey, BuildFile (..)
                   , FileSize (..), FileType (..), HasPantryConfig
                   , Mismatch (..), Package (..), PackageCabal (..)
                   , PackageIdentifier (..), PackageMetadata (..)
                   , PantryException (..), PHpack (..), RawArchive (..)
                   , RawPackageLocationImmutable (..), RawPackageMetadata (..)
                   , ResolvedPath (..), SHA256, Tree (..), TreeEntry (..)
                   , TreeKey, cabalFileName, hpackSafeFilePath, mkSafeFilePath
                   , toRawArchive, toRawPM, unSafeFilePath
                   )
import           Path ( toFilePath )
import           Path.IO ( doesFileExist )
import           RIO
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.List as List
import qualified RIO.Map as Map
import           RIO.Process ( HasProcessContext )
import qualified RIO.Set as Set
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T

fetchArchivesRaw ::
     (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => [(RawArchive, RawPackageMetadata)]
  -> RIO env ()
fetchArchivesRaw pairs =
  for_ pairs $ \(ra, rpm) ->
    getArchive (RPLIArchive ra rpm) ra rpm

fetchArchives ::
     (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => [(Archive, PackageMetadata)]
  -> RIO env ()
fetchArchives pairs =
  -- TODO be more efficient, group together shared archives
  fetchArchivesRaw [(toRawArchive a, toRawPM pm) | (a, pm) <- pairs]

getArchiveKey ::
     forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => RawPackageLocationImmutable -- ^ for exceptions
  -> RawArchive
  -> RawPackageMetadata
  -> RIO env TreeKey
getArchiveKey rpli archive rpm =
  packageTreeKey <$> getArchivePackage rpli archive rpm -- potential optimization

thd4 :: (a, b, c, d) -> c
thd4 (_, _, z, _) = z

getArchivePackage ::
     forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env, HasCallStack)
  => RawPackageLocationImmutable -- ^ for exceptions
  -> RawArchive
  -> RawPackageMetadata
  -> RIO env Package
getArchivePackage rpli archive rpm = thd4 <$> getArchive rpli archive rpm

getArchive ::
     forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env, HasCallStack)
  => RawPackageLocationImmutable -- ^ for exceptions
  -> RawArchive
  -> RawPackageMetadata
  -> RIO env (SHA256, FileSize, Package, CachedTree)
getArchive rpli archive rpm = do
  -- Check if the value is in the cache, and use it if possible
  mcached <- loadCache rpli archive
  -- Ensure that all of the blobs referenced exist in the cache
  -- See: https://github.com/commercialhaskell/pantry/issues/27
  mtree <-
    case mcached of
      Nothing -> pure Nothing
      Just (_, _, pa) -> do
        etree <- withStorage $ loadCachedTree $ packageTree pa
        case etree of
          Left e -> do
            logDebug $
                 "getArchive of "
              <> displayShow rpli
              <> ": loadCachedTree failed: "
              <> displayShow e
            pure Nothing
          Right x -> pure $ Just x
  cached@(_, _, pa, _) <-
    case (mcached, mtree) of
      (Just (a, b, c), Just d) -> pure (a, b, c, d)
      -- Not in the archive. Load the archive. Completely ignore the
      -- PackageMetadata for now, we'll check that the Package info matches
      -- next.
      _ -> withArchiveLoc archive $ \fp sha size -> do
        (pa, tree) <- parseArchive rpli archive fp
        -- Storing in the cache exclusively uses information we have about the
        -- archive itself, not metadata from the user.
        storeCache archive sha size pa
        pure (sha, size, pa, tree)

  either throwIO (\_ -> pure cached) $ checkPackageMetadata rpli rpm pa

storeCache ::
     forall env. (HasPantryConfig env, HasLogFunc env)
  => RawArchive
  -> SHA256
  -> FileSize
  -> Package
  -> RIO env ()
storeCache archive sha size pa =
  case raLocation archive of
    ALUrl url -> withStorage $
      storeArchiveCache url (raSubdir archive) sha size (packageTreeKey pa)
    ALFilePath _ -> pure () -- TODO cache local as well

loadCache ::
     forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => RawPackageLocationImmutable
  -> RawArchive
  -> RIO env (Maybe (SHA256, FileSize, Package))
loadCache rpli archive =
  case loc of
    ALFilePath _ -> pure Nothing -- TODO can we do something intelligent here?
    ALUrl url -> withStorage (loadArchiveCache url (raSubdir archive)) >>= loop
 where
  loc = raLocation archive
  msha = raHash archive
  msize = raSize archive

  loadFromCache :: TreeId -> RIO env (Maybe Package)
  loadFromCache tid = fmap Just $ withStorage $ loadPackageById rpli tid

  loop [] = pure Nothing
  loop ((sha, size, tid):rest) =
    case msha of
      Nothing -> do
        case msize of
          Just size' | size /= size' -> loop rest
          _ -> do
            case loc of
              ALUrl url -> do
                -- Only debug level, let lock files solve this
                logDebug $
                     "Using archive from "
                  <> display url
                  <> " without a specified cryptographic hash"
                logDebug $
                     "Cached hash is "
                  <> display sha
                  <> ", file size "
                  <> display size
              ALFilePath _ -> pure ()
            fmap (sha, size,) <$> loadFromCache tid
      Just sha'
        | sha == sha' ->
            case msize of
              Nothing -> do
                case loc of
                  -- Only debug level, let lock files solve this
                  ALUrl url -> logDebug $
                       "Archive from "
                    <> display url
                    <> " does not specify a size"
                  ALFilePath _ -> pure ()
                fmap (sha, size,) <$> loadFromCache tid
              Just size'
                | size == size' -> fmap (sha, size,) <$> loadFromCache tid
                | otherwise -> do
                    -- This is an actual warning, since we have a concrete
                    -- mismatch
                    logWarn $
                         "Archive from "
                      <> display loc
                      <> " has a matching hash but mismatched size"
                    logWarn "Please verify that your configuration provides \
                            \the correct size"
                    loop rest
        | otherwise -> loop rest

-- ensure name, version, etc are correct
checkPackageMetadata ::
     RawPackageLocationImmutable
  -> RawPackageMetadata
  -> Package
  -> Either PantryException Package
checkPackageMetadata pl pm pa = do
  let
      err = MismatchedPackageMetadata
              pl
              pm
              (Just (packageTreeKey pa))
              (packageIdent pa)

      test :: Eq a => Maybe a -> a -> Bool
      test (Just x) y = x == y
      test Nothing _ = True

      tests =
        [ test (rpmTreeKey pm) (packageTreeKey pa)
        , test (rpmName pm) (pkgName $ packageIdent pa)
        , test (rpmVersion pm) (pkgVersion $ packageIdent pa)
        ]

   in if and tests then Right pa else Left err

-- | Provide a local file with the contents of the archive, regardless of where
-- it comes from. If not downloading, checks that the archive file exists.
-- Performs SHA256 and file size validation.
withArchiveLoc ::
     HasLogFunc env
  => RawArchive
  -> (FilePath -> SHA256 -> FileSize -> RIO env a)
  -> RIO env a
withArchiveLoc (RawArchive (ALFilePath resolved) msha msize _subdir) f = do
  let abs' = resolvedAbsolute resolved
      fp = toFilePath abs'
  archiveExists <- doesFileExist abs'
  unless archiveExists $
    throwIO $ LocalNoArchiveFileFound abs'
  (sha, size) <- withBinaryFile fp ReadMode $ \h -> do
    size <- FileSize . fromIntegral <$> hFileSize h
    for_ msize $ \size' ->
      when (size /= size') $
        throwIO $ LocalInvalidSize abs' Mismatch
          { mismatchExpected = size'
          , mismatchActual = size
          }

    sha <- runConduit (sourceHandle h .| SHA256.sinkHash)
    for_ msha $ \sha' ->
      when (sha /= sha') $
        throwIO $ LocalInvalidSHA256 abs' Mismatch
          { mismatchExpected = sha'
          , mismatchActual = sha
          }

    pure (sha, size)
  f fp sha size
withArchiveLoc (RawArchive (ALUrl url) msha msize _subdir) f =
  withSystemTempFile "archive" $ \fp hout -> do
    logDebug $ "Downloading archive from " <> display url
    (sha, size, ()) <- httpSinkChecked url msha msize (sinkHandle hout)
    hClose hout
    f fp sha size

data ArchiveType = ATTarGz | ATTar | ATZip
  deriving (Enum, Bounded)

instance Display ArchiveType where
  display ATTarGz = "GZIP-ed tar file"
  display ATTar = "Uncompressed tar file"
  display ATZip = "Zip file"

data METype
  = METNormal
  | METExecutable
  | METLink !FilePath
  deriving Show

data MetaEntry = MetaEntry
  { mePath :: !FilePath
  , meType :: !METype
  }
  deriving Show

foldArchive ::
     (HasPantryConfig env, HasLogFunc env)
  => ArchiveLocation -- ^ for error reporting
  -> FilePath
  -> ArchiveType
  -> a
  -> (a -> MetaEntry -> ConduitT ByteString Void (RIO env) a)
  -> RIO env a
foldArchive loc fp ATTarGz accum f =
  withSourceFile fp $ \src -> runConduit $ src .| ungzip .| foldTar loc accum f
foldArchive loc fp ATTar accum f =
  withSourceFile fp $ \src -> runConduit $ src .| foldTar loc accum f
foldArchive loc fp ATZip accum0 f = withBinaryFile fp ReadMode $ \h -> do
  let go accum entry = do
        let normalizedRelPath = removeInitialDotSlash $ Zip.eRelativePath entry
            me = MetaEntry normalizedRelPath met
            met = fromMaybe METNormal $ do
              let modes = shiftR (Zip.eExternalFileAttributes entry) 16
              guard $ Zip.eVersionMadeBy entry .&. 0xFF00 == 0x0300
              guard $ modes /= 0
              Just $
                if (modes .&. 0o100) == 0
                  then METNormal
                  else METExecutable
            lbs = Zip.fromEntry entry
        let crcExpected = Zip.eCRC32 entry
            crcActual = CRC32.crc32 lbs
        when (crcExpected /= crcActual)
          $ throwIO $ CRC32Mismatch loc (Zip.eRelativePath entry) Mismatch
              { mismatchExpected = crcExpected
              , mismatchActual = crcActual
              }
        runConduit $ sourceLazy lbs .| f accum me
      isDir entry =
        case reverse $ Zip.eRelativePath entry of
          '/':_ -> True
          _ -> False
  -- We're entering lazy I/O land thanks to zip-archive.
  lbs <- BL.hGetContents h
  foldM go accum0 (filter (not . isDir) $ Zip.zEntries $ Zip.toArchive lbs)

foldTar ::
     (HasPantryConfig env, HasLogFunc env)
  => ArchiveLocation -- ^ for exceptions
  -> a
  -> (a -> MetaEntry -> ConduitT ByteString o (RIO env) a)
  -> ConduitT ByteString o (RIO env) a
foldTar loc accum0 f = do
  ref <- newIORef accum0
  Tar.untar $ toME >=> traverse_ (\me -> do
    accum <- readIORef ref
    accum' <- f accum me
    writeIORef ref $! accum')
  readIORef ref
 where
  toME :: MonadIO m => Tar.FileInfo -> m (Maybe MetaEntry)
  toME fi = do
    let exc = InvalidTarFileType loc (Tar.getFileInfoPath fi) (Tar.fileType fi)
    mmet <-
      case Tar.fileType fi of
        Tar.FTSymbolicLink bs ->
          case decodeUtf8' bs of
            Left _ -> throwIO exc
            Right text -> pure $ Just $ METLink $ T.unpack text
        Tar.FTNormal -> pure $ Just $
          if Tar.fileMode fi .&. 0o100 /= 0
            then METExecutable
            else METNormal
        Tar.FTDirectory -> pure Nothing
        _ -> throwIO exc
    pure $
      (\met -> MetaEntry
        { mePath = removeInitialDotSlash . Tar.getFileInfoPath $ fi
        , meType = met
        })
      <$> mmet

data SimpleEntry = SimpleEntry
  { seSource :: !FilePath
  , seType :: !FileType
  }
  deriving Show

removeInitialDotSlash :: FilePath -> FilePath
removeInitialDotSlash filename =
  fromMaybe filename $ List.stripPrefix "./" filename

-- | Attempt to parse the contents of the given archive in the given subdir into
-- a 'Tree'. This will not consult any caches. It will ensure that:
--
-- * The cabal file exists
--
-- * The cabal file can be parsed
--
-- * The name inside the cabal file matches the name of the cabal file itself
parseArchive ::
     forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => RawPackageLocationImmutable
  -> RawArchive
  -> FilePath -- ^ file holding the archive
  -> RIO env (Package, CachedTree)
parseArchive rpli archive fp = do
  let loc = raLocation archive
      archiveTypes :: [ArchiveType]
      archiveTypes = [minBound .. maxBound]
      getFiles :: [ArchiveType] -> RIO env (ArchiveType, Map FilePath MetaEntry)
      getFiles [] = throwIO $ UnknownArchiveType loc
      getFiles (at:ats) = do
        eres <- tryAny $
          -- foldArchive normalises filepaths in archives that begin with ./
          foldArchive loc fp at id $ \m me -> pure $ m . (me:)
        case eres of
          Left e -> do
            logDebug $ "parseArchive of " <> display at <> ": " <> displayShow e
            getFiles ats
          Right files ->
            pure (at, Map.fromList $ map (mePath &&& id) $ files [])
  (at, files) <- getFiles archiveTypes
  let toSimple :: FilePath -> MetaEntry -> Either String (Map FilePath SimpleEntry)
      toSimple key me =
        case meType me of
          METNormal ->
            Right $ Map.singleton key $ SimpleEntry (mePath me) FTNormal
          METExecutable ->
            Right $ Map.singleton key $ SimpleEntry (mePath me) FTExecutable
          METLink relDest -> do
            case relDest of
              '/':_ -> Left $ concat
                         [ "File located at "
                         , show $ mePath me
                         , " is a symbolic link to absolute path "
                         , relDest
                         ]
              _ -> Right ()
            dest0 <-
              case makeTarRelative (mePath me) relDest of
                Left e -> Left $ concat
                  [ "Error resolving relative path "
                  , relDest
                  , " from symlink at "
                  , mePath me
                  , ": "
                  , e
                  ]
                Right x -> Right x
            dest <-
              case normalizeParents dest0 of
                Left e -> Left $ concat
                  [ "Invalid symbolic link from "
                  , mePath me
                  , " to "
                  , relDest
                  , ", tried parsing "
                  , dest0
                  , ": "
                  , e
                  ]
                Right x -> Right x
            -- Check if it's a symlink to a file
            case Map.lookup dest files of
              Nothing ->
                -- Check if it's a symlink to a directory
                case findWithPrefix dest files of
                  [] -> Left $
                             "Symbolic link dest not found from "
                          ++ mePath me
                          ++ " to "
                          ++ relDest
                          ++ ", looking for "
                          ++ dest
                          ++ ".\n"
                          ++ "This may indicate that the source is a git \
                             \archive which uses git-annex.\n"
                          ++ "See https://github.com/commercialhaskell/stack/issues/4579 \
                             \for further information."
                  pairs ->
                    fmap fold $ for pairs $ \(suffix, me') ->
                      toSimple (key ++ '/' : suffix) me'
              Just me' ->
                case meType me' of
                  METNormal ->
                    Right $ Map.singleton key $ SimpleEntry dest FTNormal
                  METExecutable ->
                    Right $ Map.singleton key $ SimpleEntry dest FTExecutable
                  METLink _ ->
                    Left $
                         "Symbolic link dest cannot be a symbolic link, from "
                      ++ mePath me
                      ++ " to "
                      ++ relDest

  case fold <$> Map.traverseWithKey toSimple files of
    Left e -> throwIO $ UnsupportedTarball loc $ T.pack e
    Right files1 -> do
      let files2 = stripCommonPrefix $ Map.toList files1
          files3 = takeSubdir (raSubdir archive) files2
          toSafe (fp', a) =
            case mkSafeFilePath fp' of
              Nothing -> Left $ "Not a safe file path: " ++ show fp'
              Just sfp -> Right (sfp, a)
      case traverse toSafe files3 of
        Left e -> throwIO $ UnsupportedTarball loc $ T.pack e
        Right safeFiles -> do
          let toSave = Set.fromList $ map (seSource . snd) safeFiles
          (blobs :: Map FilePath (BlobKey, BlobId)) <-
            foldArchive loc fp at mempty $ \m me ->
              if mePath me `Set.member` toSave
                then do
                  bs <- mconcat <$> sinkList
                  (blobId, blobKey) <- lift $ withStorage $ storeBlob bs
                  pure $ Map.insert (mePath me) (blobKey, blobId) m
                else pure m
          tree :: CachedTree <-
            fmap (CachedTreeMap . Map.fromList) $ for safeFiles $ \(sfp, se) ->
              case Map.lookup (removeInitialDotSlash . seSource $ se) blobs of
                Nothing ->
                  error $ "Impossible: blob not found for: " ++ seSource se
                Just (blobKey, blobId) ->
                  pure (sfp, (TreeEntry blobKey (seType se), blobId))
          -- parse the cabal file and ensure it has the right name
          buildFile <- findCabalOrHpackFile rpli $ unCachedTree tree
          (buildFilePath, buildFileBlobKey, buildFileEntry) <- case buildFile of
            BFCabal fpath te@(TreeEntry key _) -> pure (fpath, key, te)
            BFHpack te@(TreeEntry key _) -> pure (hpackSafeFilePath, key, te)
          mbs <- withStorage $ loadBlob buildFileBlobKey
          bs <- case mbs of
            Nothing -> throwIO $
              TreeReferencesMissingBlob rpli buildFilePath buildFileBlobKey
            Just bs -> pure bs
          cabalBs <- case buildFile of
            BFCabal _ _ -> pure bs
            BFHpack _ -> snd <$> hpackToCabal rpli (unCachedTree tree)
          (_warnings, gpd) <- rawParseGPD (Left rpli) cabalBs
          let ident@(PackageIdentifier name _) = package $ packageDescription gpd
          case buildFile of
            BFCabal _ _ ->
              when (buildFilePath /= cabalFileName name) $
                throwIO $ WrongCabalFileName rpli buildFilePath name
            _ -> pure ()
          -- It's good! Store the tree, let's bounce
          (tid, treeKey') <- withStorage $ storeTree rpli ident tree buildFile
          packageCabal <- case buildFile of
            BFCabal _ _ -> pure $ PCCabalFile buildFileEntry
            BFHpack _ -> do
              cabalKey <- withStorage $ do
                hpackId <- storeHPack rpli tid
                loadCabalBlobKey hpackId
              hpackSoftwareVersion <- hpackVersion
              let cabalTreeEntry = TreeEntry cabalKey (teType buildFileEntry)
              pure
                $ PCHpack
                $ PHpack
                    { phOriginal = buildFileEntry
                    , phGenerated = cabalTreeEntry
                    , phVersion = hpackSoftwareVersion
                    }
          pure (Package
            { packageTreeKey = treeKey'
            , packageTree = unCachedTree tree
            , packageCabalEntry = packageCabal
            , packageIdent = ident
            }, tree)

-- | Find all of the files in the Map with the given directory as a prefix.
-- Directory is given without trailing slash. Returns the suffix after stripping
-- the given prefix.
findWithPrefix :: FilePath -> Map FilePath MetaEntry -> [(FilePath, MetaEntry)]
findWithPrefix dir = mapMaybe go . Map.toList
 where
  prefix = dir ++ "/"
  go (x, y) = (, y) <$> List.stripPrefix prefix x

findCabalOrHpackFile ::
     MonadThrow m
  => RawPackageLocationImmutable -- ^ for exceptions
  -> Tree
  -> m BuildFile
findCabalOrHpackFile loc (TreeMap m) = do
  let isCabalFile (sfp, _) =
        let txt = unSafeFilePath sfp
         in not ("/" `T.isInfixOf` txt) && (".cabal" `T.isSuffixOf` txt)
      isHpackFile (sfp, _) =
        let txt = unSafeFilePath sfp
         in T.pack Hpack.packageConfig == txt
      isBFCabal (BFCabal _ _) = True
      isBFCabal _ = False
      sfpBuildFile (BFCabal sfp _) = sfp
      sfpBuildFile (BFHpack _) = hpackSafeFilePath
      toBuildFile xs@(sfp, te) = let cbFile = if isCabalFile xs
                                                then Just $ BFCabal sfp te
                                                else Nothing
                                     hpFile = if isHpackFile xs
                                                then Just $ BFHpack te
                                                else Nothing
                                 in  cbFile <|> hpFile
  case mapMaybe toBuildFile $ Map.toList m of
    [] -> throwM $ TreeWithoutCabalFile loc
    [bfile] -> pure bfile
    xs -> case filter isBFCabal xs of
            [] -> throwM $ TreeWithoutCabalFile loc
            [bfile] -> pure bfile
            xs' -> throwM $ TreeWithMultipleCabalFiles loc $ map sfpBuildFile xs'

-- | If all files have a shared prefix, strip it off
stripCommonPrefix :: [(FilePath, a)] -> [(FilePath, a)]
stripCommonPrefix [] = []
stripCommonPrefix pairs@((firstFP, _):_) = fromMaybe pairs $ do
  let firstDir = takeWhile (/= '/') firstFP
  guard $ not $ null firstDir
  let strip (fp, a) = (, a) <$> List.stripPrefix (firstDir ++ "/") fp
  stripCommonPrefix <$> traverse strip pairs

-- | Take us down to the specified subdirectory
takeSubdir ::
     Text -- ^ subdir
  -> [(FilePath, a)] -- ^ files after stripping common prefix
  -> [(Text, a)]
takeSubdir subdir = mapMaybe $ \(fp, a) -> do
  stripped <- List.stripPrefix subdirs $ splitDirs $ T.pack fp
  Just (T.intercalate "/" stripped, a)
  where
    splitDirs = List.dropWhile (== ".") . filter (/= "") . T.splitOn "/"
    subdirs = splitDirs subdir
