{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

module Pantry.Types
  ( PantryConfig (..)
  , PackageIndexConfig (..)
  , HackageSecurityConfig (..)
  , defaultHackageSecurityConfig
  , Storage (..)
  , HasPantryConfig (..)
  , BlobKey (..)
  , PackageName
  , Version
  , PackageIdentifier (..)
  , Revision (..)
  , ModuleName
  , CabalFileInfo (..)
  , PrintWarnings (..)
  , PackageNameP (..)
  , VersionP (..)
  , ModuleNameP (..)
  , PackageIdentifierRevision (..)
  , pirForHash
  , FileType (..)
  , BuildFile (..)
  , FileSize (..)
  , TreeEntry (..)
  , SafeFilePath
  , unSafeFilePath
  , mkSafeFilePath
  , safeFilePathToPath
  , hpackSafeFilePath
  , TreeKey (..)
  , Tree (..)
  , renderTree
  , parseTree
  , parseTreeM
  , SHA256
  , Unresolved
  , resolvePaths
  , Package (..)
  , PackageCabal (..)
  , PHpack (..)
  -- , PackageTarball (..)
  , RawPackageLocation (..)
  , PackageLocation (..)
  , toRawPL
  , RawPackageLocationImmutable (..)
  , PackageLocationImmutable (..)
  , toRawPLI
  , RawArchive (..)
  , Archive (..)
  , toRawArchive
  , Repo (..)
  , AggregateRepo (..)
  , SimpleRepo (..)
  , toAggregateRepos
  , rToSimpleRepo
  , arToSimpleRepo
  , RepoType (..)
  , parsePackageIdentifier
  , parsePackageName
  , parsePackageNameThrowing
  , parseFlagName
  , parseVersion
  , parseVersionThrowing
  , packageIdentifierString
  , packageNameString
  , flagNameString
  , versionString
  , moduleNameString
  , OptionalSubdirs (..)
  , ArchiveLocation (..)
  , RelFilePath (..)
  , CabalString (..)
  , toCabalStringMap
  , unCabalStringMap
  , parsePackageIdentifierRevision
  , Mismatch (..)
  , PantryException (..)
  , FuzzyResults (..)
  , ResolvedPath (..)
  , HpackExecutable (..)
  , WantedCompiler (..)
  --, resolveSnapshotLocation
  , snapshotLocation
  , defaultSnapshotLocation
  , SnapName (..)
  , parseSnapName
  , RawSnapshotLocation (..)
  , SnapshotLocation (..)
  , toRawSL
  , parseHackageText
  , parseRawSnapshotLocation
  , RawSnapshotLayer (..)
  , SnapshotLayer (..)
  , toRawSnapshotLayer
  , RawSnapshot (..)
  , Snapshot (..)
  , RawSnapshotPackage (..)
  , SnapshotPackage (..)
  , parseWantedCompiler
  , RawPackageMetadata (..)
  , PackageMetadata (..)
  , toRawPM
  , cabalFileName
  , SnapshotCacheHash (..)
  , getGlobalHintsFile
  , bsToBlobKey
  , warnMissingCabalFile
  , connRDBMS
  ) where

import           Casa.Client ( CasaRepoPrefix )
import           Data.Aeson.Encoding.Internal ( unsafeToEncoding )
import           Data.Aeson.Types ( Parser, toJSONKeyText )
import           Data.ByteString.Builder
                   ( byteString, toLazyByteString, wordDec )
import qualified Data.List.NonEmpty as NE
import           Data.Text.Read ( decimal )
import           Distribution.CabalSpecVersion ( cabalSpecLatest )
#if MIN_VERSION_Cabal(3,4,0)
import           Distribution.CabalSpecVersion ( cabalSpecToVersionDigits )
#else
import           Distribution.CabalSpecVersion ( CabalSpecVersion (..) )
#endif
import           Distribution.ModuleName ( ModuleName )
import           Distribution.PackageDescription
                   ( FlagName, GenericPackageDescription, unFlagName )
import           Distribution.Parsec
                   ( PError (..), PWarning (..), ParsecParser
                   , explicitEitherParsec, parsec, showPos
                   )
import qualified Distribution.Pretty
import qualified Distribution.Text
import           Distribution.Types.PackageId ( PackageIdentifier (..) )
import           Distribution.Types.PackageName
                   ( PackageName, mkPackageName, unPackageName )
import           Distribution.Types.Version ( Version, mkVersion, nullVersion )
import           Distribution.Types.VersionRange ( VersionRange )
import qualified Data.Conduit.Tar as Tar
import qualified Data.Map.Strict as Map ( mapKeysMonotonic )
import           Database.Persist
import           Database.Persist.Sql
#if MIN_VERSION_persistent(2, 13, 0)
import           Database.Persist.SqlBackend.Internal ( connRDBMS )
#endif
import qualified Distribution.Compat.CharParsing as Parse
import qualified Hpack.Config as Hpack
import           Network.HTTP.Client ( parseRequest )
import           Network.HTTP.Types ( Status, statusCode )
import           Pantry.Internal.AesonExtended
                   ( FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..)
                   , Object, ToJSON (..), ToJSONKey (..), ToJSONKeyFunction (..)
                   , Value (..), WarningParser, WithJSONWarnings, (..:), (..:?)
                   , (..!=), (.=), (.:), (...:?), jsonSubWarnings
                   , jsonSubWarningsT, noJSONWarnings, object, tellJSONField
                   , withObject, withObjectWarnings, withText
                   )
import           Pantry.SHA256 ( SHA256 )
import qualified Pantry.SHA256 as SHA256
import           Path
                   ( Abs, Dir, File, Path, (</>), filename, parseRelFile
                   , toFilePath
                   )
import           Path.IO ( resolveDir, resolveFile )
import qualified RIO.Set as Set
import           RIO
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import           RIO.List ( groupBy, intersperse )
import qualified RIO.Text as T
import           RIO.Time ( Day, UTCTime, toGregorian )
import qualified RIO.Map as Map
import           RIO.PrettyPrint
                   ( bulletedList, fillSep, flow, hang, line, mkNarrativeList
                   , parens, string, style
                   )
import           RIO.PrettyPrint.Types ( Style (..) )
import           Text.PrettyPrint.Leijen.Extended ( Pretty (..), StyleDoc )

#if MIN_VERSION_aeson(2, 0, 0)
import qualified Data.Aeson.KeyMap as HM
import qualified Data.Aeson.Key

type AesonKey = Data.Aeson.Key.Key
#else
import qualified RIO.HashMap as HM

type AesonKey = Text
#endif

-- | Parsed tree with more information on the Haskell package it contains.
--
-- @since 0.1.0.0
data Package = Package
  { packageTreeKey :: !TreeKey
    -- ^ The 'TreeKey' containing this package.
    --
    -- This is a hash of the binary representation of 'packageTree'.
    --
    -- @since 0.1.0.0
  , packageTree :: !Tree
    -- ^ The 'Tree' containing this package.
    --
    -- @since 0.1.0.0
  , packageCabalEntry :: !PackageCabal
    -- ^ Information on the cabal file inside this package.
    --
    -- @since 0.1.0.0
  , packageIdent :: !PackageIdentifier
    -- ^ The package name and version in this package.
    --
    -- @since 0.1.0.0
  }
  deriving (Show, Eq, Ord)

data PHpack = PHpack
  { phOriginal :: !TreeEntry
    -- ^ Original hpack file
  , phGenerated :: !TreeEntry
    -- ^ Generated Cabal file
  , phVersion :: !Version
    -- ^ Version of Hpack used
  }
  deriving (Show, Eq, Ord)

data PackageCabal
  = PCCabalFile !TreeEntry -- ^ TreeEntry of Cabal file
  | PCHpack !PHpack
  deriving (Show, Eq, Ord)

cabalFileName :: PackageName -> SafeFilePath
cabalFileName name =
  case mkSafeFilePath $ T.pack (packageNameString name) <> ".cabal" of
    Nothing -> error $ "cabalFileName: failed for " ++ show name
    Just sfp -> sfp

-- | The revision number of a package from Hackage, counting upwards from 0 (the
-- original cabal file).
--
-- See caveats on 'CFIRevision'.
--
-- @since 0.1.0.0
newtype Revision = Revision Word
  deriving (Generic, Show, Eq, NFData, Data, Typeable, Ord, Hashable, Display, PersistField, PersistFieldSql)

-- | Represents a SQL database connection. This used to be a newtype wrapper
-- around a connection pool. However, when investigating
-- <https://github.com/commercialhaskell/stack/issues/4471>, it appeared that
-- holding a pool resulted in overly long write locks being held on the
-- database. As a result, we now abstract away whether a pool is used, and the
-- default implementation in "Pantry.Storage" does not use a pool.
data Storage = Storage
  { withStorage_ ::
         forall env a. HasLogFunc env
      => ReaderT SqlBackend (RIO env) a
      -> RIO env a
  , withWriteLock_ :: forall env a. HasLogFunc env => RIO env a -> RIO env a
  }

-- | Configuration value used by the entire pantry package. Create one using
-- 'withPantryConfig' or 'withPantryConfig''. See also 'PantryApp' for a
-- convenience approach to using pantry.
--
-- @since 0.1.0.0
data PantryConfig = PantryConfig
  { pcPackageIndex :: !PackageIndexConfig
  , pcHpackExecutable :: !HpackExecutable
  , pcRootDir :: !(Path Abs Dir)
  , pcStorage :: !Storage
  , pcUpdateRef :: !(MVar Bool)
    -- ^ Want to try updating the index once during a single run for missing
    -- package identifiers. We also want to ensure we only update once at a
    -- time. Start at @True@.
  , pcParsedCabalFilesRawImmutable ::
      !(IORef (Map RawPackageLocationImmutable GenericPackageDescription))
    -- ^ Cache of previously parsed cabal files, to save on slow parsing time.
  , pcParsedCabalFilesMutable ::
      !( IORef
           ( Map
             (Path Abs Dir)
             ( PrintWarnings -> IO GenericPackageDescription
             , PackageName
             , Path Abs File
             )
           )
       )
    -- ^ Cache for mutable packages. We want to allow for an optimization:
    -- deferring parsing of the 'GenericPackageDescription' until its actually
    -- needed. Therefore, we keep the filepath and the 'PackageName' derived
    -- from that filepath. When the @IO GenericPackageDescription@ is run, it
    -- will ensure that the @PackageName@ matches the value inside the cabal
    -- file, and print out any warnings that still need to be printed.
  , pcConnectionCount :: !Int
    -- ^ concurrently open downloads
  , pcCasaConfig :: !(Maybe (CasaRepoPrefix, Int))
    -- ^ Optionally, the Casa pull URL e.g. @https://casa.fpcomplete.com@ and
    -- the maximum number of Casa keys to pull per request.
  , pcSnapshotLocation :: SnapName -> RawSnapshotLocation
    -- ^ The location of snapshot synonyms
  }

-- | Get the location of a snapshot synonym from the 'PantryConfig'.
--
-- @since 0.5.0.0
snapshotLocation ::
     HasPantryConfig env
  => SnapName
  -> RIO env RawSnapshotLocation
snapshotLocation name = do
  loc <- view $ pantryConfigL.to pcSnapshotLocation
  pure $ loc name

-- | Should we print warnings when loading a cabal file?
--
-- @since 0.1.0.0
data PrintWarnings = YesPrintWarnings | NoPrintWarnings

-- | Wraps a value which potentially contains relative paths. Needs to be
-- provided with a base directory to resolve these paths.
--
-- Unwrap this using 'resolvePaths'.
--
-- @since 0.1.0.0
newtype Unresolved a = Unresolved (Maybe (Path Abs Dir) -> IO a)
  deriving Functor

instance Applicative Unresolved where
  pure = Unresolved . const . pure
  Unresolved f <*> Unresolved x = Unresolved $ \mdir -> f mdir <*> x mdir

-- | Resolve all of the file paths in an 'Unresolved' relative to the given
-- directory.
--
-- @since 0.1.0.0
resolvePaths ::
     MonadIO m
  => Maybe (Path Abs Dir) -- ^ directory to use for relative paths
  -> Unresolved a
  -> m a
resolvePaths mdir (Unresolved f) = liftIO (f mdir)

-- | A combination of the relative path provided in a config file, together with
-- the resolved absolute path.
--
-- @since 0.1.0.0
data ResolvedPath t = ResolvedPath
  { resolvedRelative :: !RelFilePath
    -- ^ Original value parsed from a config file.
  , resolvedAbsolute :: !(Path Abs t)
    -- ^ Absolute path resolved against base directory loaded from.
  }
  deriving (Show, Eq, Generic, Ord)

instance NFData (ResolvedPath t)

-- | Location to load a package from. Can either be immutable (see
-- 'PackageLocationImmutable') or a local directory which is expected to change
-- over time. Raw version doesn't include exact package version (e.g. could
-- refer to the latest revision on Hackage)
--
-- @since 0.1.0.0
data RawPackageLocation
  = RPLImmutable !RawPackageLocationImmutable
  | RPLMutable !(ResolvedPath Dir)
  deriving (Show, Eq, Generic)

instance NFData RawPackageLocation

-- | Location to load a package from. Can either be immutable (see
-- 'PackageLocationImmutable') or a local directory which is expected to change
-- over time.
--
-- @since 0.1.0.0
data PackageLocation
  = PLImmutable !PackageLocationImmutable
  | PLMutable !(ResolvedPath Dir)
  deriving (Show, Eq, Generic)

instance NFData PackageLocation

instance Display PackageLocation where
  display (PLImmutable loc) = display loc
  display (PLMutable fp) = fromString $ toFilePath $ resolvedAbsolute fp

-- | Convert `PackageLocation` to its "raw" equivalent
--
-- @since 0.1.0.0
toRawPL :: PackageLocation -> RawPackageLocation
toRawPL (PLImmutable im) = RPLImmutable (toRawPLI im)
toRawPL (PLMutable m) = RPLMutable m

-- | Location for remote packages or archives assumed to be immutable.
-- as user specifies it i.e. not an exact location
--
-- @since 0.1.0.0
data RawPackageLocationImmutable
  = RPLIHackage !PackageIdentifierRevision !(Maybe TreeKey)
  | RPLIArchive !RawArchive !RawPackageMetadata
  | RPLIRepo    !Repo !RawPackageMetadata
  deriving (Show, Eq, Ord, Generic)

instance NFData RawPackageLocationImmutable

instance Display RawPackageLocationImmutable where
  display (RPLIHackage pir _tree) = display pir <> " (from Hackage)"
  display (RPLIArchive archive _pm) =
    "Archive from " <> display (raLocation archive) <>
    (if T.null $ raSubdir archive
       then mempty
       else " in subdir " <> display (raSubdir archive))
  display (RPLIRepo repo _pm) =
    "Repo from " <> display (repoUrl repo) <>
    ", commit " <> display (repoCommit repo) <>
    (if T.null $ repoSubdir repo
       then mempty
       else " in subdir " <> display (repoSubdir repo))

instance Pretty RawPackageLocationImmutable where
  pretty (RPLIHackage pir _tree) = fillSep
    [ fromString . T.unpack $ textDisplay pir
    , parens (flow "from Hackage")
    ]
  pretty (RPLIArchive archive _pm) = fillSep
    [ flow "Archive from"
    , pretty (raLocation archive)
    , if T.null $ raSubdir archive
        then mempty
        else fillSep
          [ flow "in subdir"
          , style Dir (fromString $ T.unpack (raSubdir archive))
          ]
    ]
  pretty (RPLIRepo repo _pm) = fillSep
    [ flow "Repo from"
    , style Url (fromString $ T.unpack (repoUrl repo)) <> ","
    , "commit"
    , fromString $ T.unpack (repoCommit repo)
    , if T.null $ repoSubdir repo
        then mempty
        else fillSep
          [ flow "in subdir"
          , style Dir (fromString $ T.unpack (repoSubdir repo))
          ]
    ]

-- | Location for remote packages or archives assumed to be immutable.
--
-- @since 0.1.0.0
data PackageLocationImmutable
  = PLIHackage !PackageIdentifier !BlobKey !TreeKey
  | PLIArchive !Archive !PackageMetadata
  | PLIRepo    !Repo !PackageMetadata
  deriving (Generic, Show, Eq, Ord, Typeable)

instance NFData PackageLocationImmutable

instance Display PackageLocationImmutable where
  display (PLIHackage ident _cabalHash _tree) =
    fromString (packageNameString $ pkgName ident) <> " (from Hackage)"
  display (PLIArchive archive _pm) =
    "Archive from " <> display (archiveLocation archive) <>
    (if T.null $ archiveSubdir archive
       then mempty
       else " in subdir " <> display (archiveSubdir archive))
  display (PLIRepo repo _pm) =
    "Repo from " <> display (repoUrl repo) <>
    ", commit " <> display (repoCommit repo) <>
    (if T.null $ repoSubdir repo
       then mempty
       else " in subdir " <> display (repoSubdir repo))

instance ToJSON PackageLocationImmutable where
  toJSON = toJSON . toRawPLI

-- | Package identifier and revision with a specified cabal file hash
--
-- @since 0.1.0.0
pirForHash :: PackageIdentifier -> BlobKey -> PackageIdentifierRevision
pirForHash (PackageIdentifier name ver) (BlobKey sha size') =
  let cfi = CFIHash sha (Just size')
  in PackageIdentifierRevision name ver cfi

-- | Convert `PackageLocationImmutable` to its "raw" equivalent
--
-- @since 0.1.0.0
toRawPLI :: PackageLocationImmutable -> RawPackageLocationImmutable
toRawPLI (PLIHackage ident cfKey treeKey) =
  RPLIHackage (pirForHash ident cfKey) (Just treeKey)
toRawPLI (PLIArchive archive pm) =
  RPLIArchive (toRawArchive archive) (toRawPM pm)
toRawPLI (PLIRepo repo pm) = RPLIRepo repo (toRawPM pm)

-- | A raw package archive, specified by a user, could have no
-- hash and file size information.
--
-- @since 0.1.0.0
data RawArchive = RawArchive
  { raLocation :: !ArchiveLocation
    -- ^ Location of the archive
    --
    -- @since 0.1.0.0
  , raHash :: !(Maybe SHA256)
    -- ^ Cryptographic hash of the archive file
    --
    -- @since 0.1.0.0
  , raSize :: !(Maybe FileSize)
    -- ^ Size of the archive file
    --
    -- @since 0.1.0.0
  , raSubdir :: !Text
    -- ^ Subdirectory within the archive to get the package from.
    --
    -- @since 0.1.0.0
  }
  deriving (Generic, Show, Eq, Ord, Typeable)

instance NFData RawArchive

-- | A package archive, could be from a URL or a local file path. Local file
-- path archives are assumed to be unchanging over time, and so are allowed in
-- custom snapshots.
--
-- @since 0.1.0.0
data Archive = Archive
  { archiveLocation :: !ArchiveLocation
    -- ^ Location of the archive
    --
    -- @since 0.1.0.0
  , archiveHash :: !SHA256
    -- ^ Cryptographic hash of the archive file
    --
    -- @since 0.1.0.0
  , archiveSize :: !FileSize
    -- ^ Size of the archive file
    --
    -- @since 0.1.0.0
  , archiveSubdir :: !Text
    -- ^ Subdirectory within the archive to get the package from.
    --
    -- @since 0.1.0.0
  }
  deriving (Generic, Show, Eq, Ord, Typeable)

instance NFData Archive

-- | Convert archive to its "raw" equivalent.
--
-- @since 0.1.0.0
toRawArchive :: Archive -> RawArchive
toRawArchive archive =
  RawArchive (archiveLocation archive) (Just $ archiveHash archive)
             (Just $ archiveSize archive) (archiveSubdir archive)

-- | The type of a source control repository.
--
-- @since 0.1.0.0
data RepoType = RepoGit | RepoHg
  deriving (Generic, Show, Eq, Ord, Typeable)

instance NFData RepoType

instance PersistField RepoType where
  toPersistValue RepoGit = toPersistValue (1 :: Int32)
  toPersistValue RepoHg = toPersistValue (2 :: Int32)
  fromPersistValue v = do
    i <- fromPersistValue v
    case i :: Int32 of
      1 -> pure RepoGit
      2 -> pure RepoHg
      _ -> Left $ fromString $ "Invalid RepoType: " ++ show i

instance PersistFieldSql RepoType where
  sqlType _ = SqlInt32

-- | Information on packages stored in a source control repository.
--
-- @since 0.1.0.0
data Repo = Repo
  { repoUrl :: !Text
    -- ^ Location of the repo
    --
    -- @since 0.1.0.0
  , repoCommit :: !Text
    -- ^ Commit to use from the repo. It's strongly recommended to use
    -- a hash instead of a tag or branch name.
    --
    -- @since 0.1.0.0
  , repoType :: !RepoType
    -- ^ The type of the repo
    --
    -- @since 0.1.0.0
  , repoSubdir :: !Text
    -- ^ Subdirectory within the archive to get the package from.
    --
    -- @since 0.1.0.0
  }
  deriving (Generic, Eq, Ord, Typeable)

instance NFData Repo

instance Show Repo where
  show = T.unpack . utf8BuilderToText . display

instance Display Repo where
  display (Repo url commit typ subdir) =
    (case typ of
       RepoGit -> "Git"
       RepoHg -> "Mercurial") <>
    " repo at " <>
    display url <>
    ", commit " <>
    display commit <>
    (if T.null subdir
      then mempty
      else " in subdirectory " <> display subdir)

rToSimpleRepo :: Repo -> SimpleRepo
rToSimpleRepo Repo {..} = SimpleRepo
  { sRepoUrl = repoUrl
  , sRepoCommit = repoCommit
  , sRepoType = repoType
  }

data AggregateRepo = AggregateRepo
  { aRepo :: !SimpleRepo
  , aRepoSubdirs :: [(Text, RawPackageMetadata)]
  }
  deriving (Show, Generic, Eq, Ord, Typeable)

-- | Group input repositories by non-subdir values.
toAggregateRepos :: [(Repo, RawPackageMetadata)] -> [AggregateRepo]
toAggregateRepos = mapMaybe toAggregateRepo . groupBy matchRepoExclSubdir
 where
  toAggregateRepo :: [(Repo, RawPackageMetadata)] -> Maybe AggregateRepo
  toAggregateRepo [] = Nothing
  toAggregateRepo xs@((repo, _):_) =
    Just $ AggregateRepo (rToSimpleRepo repo) (fmap (first repoSubdir) xs)

  matchRepoExclSubdir x1 x2 =
    let (Repo url1 commit1 type1 _, _) = x1
        (Repo url2 commit2 type2 _, _) = x2
    in  (url1, commit1, type1) == (url2, commit2, type2)

arToSimpleRepo :: AggregateRepo -> SimpleRepo
arToSimpleRepo AggregateRepo {..} = aRepo

-- | Repository without subdirectory information.
--
-- @since 0.5.3
data SimpleRepo = SimpleRepo
  { sRepoUrl :: !Text
  , sRepoCommit :: !Text
  , sRepoType :: !RepoType
  }
  deriving (Show, Generic, Eq, Ord, Typeable)

instance Display SimpleRepo where
  display (SimpleRepo url commit typ) =
    (case typ of
       RepoGit -> "Git"
       RepoHg -> "Mercurial") <>
    " repo at " <>
    display url <>
    ", commit " <>
    display commit

-- An unexported newtype wrapper to hang a 'FromJSON' instance off of. Contains
-- a GitHub user and repo name separated by a forward slash, e.g. "foo/bar".
newtype GitHubRepo = GitHubRepo Text

instance FromJSON GitHubRepo where
  parseJSON = withText "GitHubRepo" $ \s -> do
    case T.split (== '/') s of
      [x, y] | not (T.null x || T.null y) -> pure (GitHubRepo s)
      _ -> fail "expecting \"user/repo\""

-- | Configuration to securely download package metadata and contents. For most
-- purposes, you'll want to use the default Hackage settings via
-- @defaultPackageIndexConfig@.
--
-- /NOTE/ It's highly recommended to only use the official Hackage
-- server or a mirror. See
-- <https://github.com/commercialhaskell/stack/issues/4137>.
--
-- @since 0.6.0
data PackageIndexConfig = PackageIndexConfig
  { picDownloadPrefix :: !Text
  , picHackageSecurityConfig :: !HackageSecurityConfig
  }
  deriving Show

-- | If the @hackage-security@ key is absent from the JSON object, assigns
-- default value 'defaultHackageSecurityConfig'.
--
-- @since 0.6.0
instance FromJSON (WithJSONWarnings PackageIndexConfig) where
  parseJSON = withObjectWarnings "PackageIndexConfig" $ \o -> do
    picDownloadPrefix <- o ..: "download-prefix"
    picHackageSecurityConfig <- jsonSubWarnings $
      o ..:? "hackage-security" ..!= noJSONWarnings defaultHackageSecurityConfig
    pure PackageIndexConfig {..}

-- | Default 'HackageSecurityConfig' value using the official Hackage server.
-- The value of the 'hscIgnoreExpiry' field is 'True'.
--
-- @since 0.7.0
defaultHackageSecurityConfig :: HackageSecurityConfig
defaultHackageSecurityConfig = HackageSecurityConfig
  { hscKeyIds =
      -- Key owners and public keys are provided as a convenience to readers.
      -- The canonical source for this mapping data is the hackage-root-keys
      -- repository and Hackage's root.json file.
      --
      -- Links:
      --  * https://github.com/haskell-infra/hackage-root-keys
      --  * https://hackage.haskell.org/root.json
      -- Please consult root.json on Hackage to map key IDs to public keys,
      -- and the hackage-root-keys repository to map public keys to their
      -- owners.
      [ -- Adam Gundry (uRPdSiL3/MNsk50z6NB55ABo0OrrNDXigtCul4vtzmw=)
        "fe331502606802feac15e514d9b9ea83fee8b6ffef71335479a2e68d84adc6b0"
      , -- Gershom Bazerman (bYoUXXQ9TtX10UriaMiQtTccuXPGnmldP68djzZ7cLo=)
        "1ea9ba32c526d1cc91ab5e5bd364ec5e9e8cb67179a471872f6e26f0ae773d42"
      , -- John Wiegley (zazm5w480r+zPO6Z0+8fjGuxZtb9pAuoVmQ+VkuCvgU=)
        "0a5c7ea47cd1b15f01f5f51a33adda7e655bc0f0b0615baa8e271f4c3351e21d"
      , -- Norman Ramsey (ZI8di3a9Un0s2RBrt5GwVRvfOXVuywADfXGPZfkiDb0=)
        "51f0161b906011b52c6613376b1ae937670da69322113a246a09f807c62f6921"
      , -- Mathieu Boespflug (ydN1nGGQ79K1Q0nN+ul+Ln8MxikTB95w0YdGd3v3kmg=)
        "be75553f3c7ba1dbe298da81f1d1b05c9d39dd8ed2616c9bddf1525ca8c03e48"
      , -- Joachim Breitner (5iUgwqZCWrCJktqMx0bBMIuoIyT4A1RYGozzchRN9rA=)
        "d26e46f3b631aae1433b89379a6c68bd417eb5d1c408f0643dcc07757fece522"
      ]
  , hscKeyThreshold = 3
  , hscIgnoreExpiry = True
  }

-- | Configuration for Hackage Security to securely download package metadata
-- and contents. For most purposes, you'll want to use the default Hackage
-- settings via @defaultHackageSecurityConfig@.
--
-- /NOTE/ It's highly recommended to only use the official Hackage
-- server or a mirror. See
-- <https://github.com/commercialhaskell/stack/issues/4137>.
--
-- @since 0.6.0
data HackageSecurityConfig = HackageSecurityConfig
  { hscKeyIds :: ![Text]
  , hscKeyThreshold :: !Int
  , hscIgnoreExpiry :: !Bool
  }
  deriving Show

-- | If the @ignore-expiry@ key is absent from the JSON object, assigns default
-- value 'True'.
--
-- @since 0.1.1.0
instance FromJSON (WithJSONWarnings HackageSecurityConfig) where
  parseJSON = withObjectWarnings "HackageSecurityConfig" $ \o -> do
    hscKeyIds <- o ..: "keyids"
    hscKeyThreshold <- o ..: "key-threshold"
    hscIgnoreExpiry <- o ..:? "ignore-expiry" ..!= True
    pure HackageSecurityConfig {..}

-- | An environment which contains a 'PantryConfig'.
--
-- @since 0.1.0.0
class HasPantryConfig env where
  -- | Lens to get or set the 'PantryConfig'
  --
  -- @since 0.1.0.0
  pantryConfigL :: Lens' env PantryConfig


-- | File size in bytes
--
-- @since 0.1.0.0
newtype FileSize = FileSize Word
  deriving ( Show, Eq, Ord, Typeable, Generic, Display, Hashable, NFData
           , PersistField, PersistFieldSql, ToJSON, FromJSON
           )

-- | A key for looking up a blob, which combines the SHA256 hash of the contents
-- and the file size.
--
-- The file size may seem redundant with the hash. However, it is necessary for
-- safely downloading blobs from an untrusted source. See
-- <https://www.fpcomplete.com/blog/2018/07/pantry-part-2-trees-keys>.
--
-- @since 0.1.0.0
data BlobKey = BlobKey !SHA256 !FileSize
  deriving (Eq, Ord, Typeable, Generic)

instance NFData BlobKey

instance Show BlobKey where
  show = T.unpack . utf8BuilderToText . display

instance Display BlobKey where
  display (BlobKey sha size') = display sha <> "," <> display size'

blobKeyPairs :: BlobKey -> [(AesonKey, Value)]
blobKeyPairs (BlobKey sha size') =
  [ "sha256" .= sha
  , "size" .= size'
  ]

instance ToJSON BlobKey where
  toJSON = object . blobKeyPairs

instance FromJSON BlobKey where
  parseJSON = withObject "BlobKey" $ \o -> BlobKey
    <$> o .: "sha256"
    <*> o .: "size"

newtype PackageNameP = PackageNameP { unPackageNameP :: PackageName }
  deriving (Eq, Ord, Show, Read, NFData)

instance Display PackageNameP where
  display = fromString . packageNameString . unPackageNameP

instance PersistField PackageNameP where
  toPersistValue (PackageNameP pn) = PersistText $ T.pack $ packageNameString pn
  fromPersistValue v = do
    str <- fromPersistValue v
    case parsePackageName str of
      Nothing -> Left $ "Invalid package name: " <> T.pack str
      Just pn -> Right $ PackageNameP pn

instance PersistFieldSql PackageNameP where
  sqlType _ = SqlString

instance ToJSON PackageNameP where
  toJSON (PackageNameP pn) = String $ T.pack $ packageNameString pn

instance FromJSON PackageNameP where
  parseJSON =
    withText "PackageNameP" $ pure . PackageNameP . mkPackageName . T.unpack

instance ToJSONKey PackageNameP where
  toJSONKey =
    ToJSONKeyText
      (fromString . packageNameString . unPackageNameP)
      (unsafeToEncoding . getUtf8Builder . display)

instance FromJSONKey PackageNameP where
  fromJSONKey = FromJSONKeyText $ PackageNameP . mkPackageName . T.unpack

newtype VersionP = VersionP { unVersionP :: Version }
  deriving (Eq, Ord, Show, Read, NFData)

instance PersistField VersionP where
  toPersistValue (VersionP v) = PersistText $ T.pack $ versionString v
  fromPersistValue v = do
    str <- fromPersistValue v
    case parseVersion str of
      Nothing -> Left $ "Invalid version number: " <> T.pack str
      Just ver -> Right $ VersionP ver

instance PersistFieldSql VersionP where
  sqlType _ = SqlString

instance Display VersionP where
  display (VersionP v) = fromString $ versionString v

instance ToJSON VersionP where
  toJSON (VersionP v) = String $ T.pack $ versionString v

instance FromJSON VersionP where
  parseJSON =
    withText "VersionP" $
    either (fail . displayException) (pure . VersionP) . parseVersionThrowing . T.unpack

newtype ModuleNameP = ModuleNameP
  { unModuleNameP :: ModuleName
  }
  deriving (Eq, Ord, Show, NFData)

instance Display ModuleNameP where
  display = fromString . moduleNameString . unModuleNameP

instance PersistField ModuleNameP where
  toPersistValue (ModuleNameP mn) = PersistText $ T.pack $ moduleNameString mn
  fromPersistValue v = do
    str <- fromPersistValue v
    case parseModuleName str of
      Nothing -> Left $ "Invalid module name: " <> T.pack str
      Just pn -> Right $ ModuleNameP pn

instance PersistFieldSql ModuleNameP where
  sqlType _ = SqlString

-- | How to choose a cabal file for a package from Hackage. This is to work with
-- Hackage cabal file revisions, which makes @PackageIdentifier@ insufficient
-- for specifying a package from Hackage.
--
-- @since 0.1.0.0
data CabalFileInfo
  = CFILatest
    -- ^ Take the latest revision of the cabal file available. This
    -- isn't reproducible at all, but the running assumption (not
    -- necessarily true) is that cabal file revisions do not change
    -- semantics of the build.
    --
    -- @since 0.1.0.0
  | CFIHash !SHA256 !(Maybe FileSize)
    -- ^ Identify by contents of the cabal file itself. Only reason for
    -- @Maybe@ on @FileSize@ is for compatibility with input that
    -- doesn't include the file size.
    --
    -- @since 0.1.0.0
  | CFIRevision !Revision
    -- ^ Identify by revision number, with 0 being the original and
    -- counting upward. This relies on Hackage providing consistent
    -- versioning. @CFIHash@ should be preferred wherever possible for
    -- reproducibility.
    --
    -- @since 0.1.0.0
  deriving (Generic, Show, Eq, Ord, Typeable)

instance NFData CabalFileInfo

instance Hashable CabalFileInfo

instance Display CabalFileInfo where
  display CFILatest = mempty
  display (CFIHash hash' msize) =
    "@sha256:" <> display hash' <> maybe mempty (\i -> "," <> display i) msize
  display (CFIRevision rev) = "@rev:" <> display rev

-- | A full specification for a package from Hackage, including the package
-- name, version, and how to load up the correct cabal file revision.
--
-- @since 0.1.0.0
data PackageIdentifierRevision
  = PackageIdentifierRevision !PackageName !Version !CabalFileInfo
  deriving (Generic, Eq, Ord, Typeable)

instance NFData PackageIdentifierRevision

instance Show PackageIdentifierRevision where
  show = T.unpack . utf8BuilderToText . display

instance Display PackageIdentifierRevision where
  display (PackageIdentifierRevision name version cfi) =
       fromString (packageNameString name)
    <> "-"
    <> fromString (versionString version)
    <> display cfi

instance ToJSON PackageIdentifierRevision where
  toJSON = toJSON . utf8BuilderToText . display

instance FromJSON PackageIdentifierRevision where
  parseJSON = withText "PackageIdentifierRevision" $ \t ->
    case parsePackageIdentifierRevision t of
      Left e -> fail $ show e
      Right pir -> pure pir

-- | Parse a hackage text.
--
-- @since 0.1.0.0
parseHackageText :: Text -> Either PantryException (PackageIdentifier, BlobKey)
parseHackageText t =
  either (\x -> error (show x) $ const $ Left $ PackageIdentifierRevisionParseFail t) Right $
  explicitEitherParsec (hackageTextParsec <* Parse.eof) $
  T.unpack t

hackageTextParsec :: ParsecParser (PackageIdentifier, BlobKey)
hackageTextParsec = do
  ident <- packageIdentifierParsec
  _ <- Parse.string "@sha256:"

  shaT <- Parse.munch (/= ',')
  sha <- either (const mzero) pure $ SHA256.fromHexText $ fromString shaT

  _ <- Parse.char ','
  size' <- Parse.integral -- FIXME probably need to handle overflow, since unfortunately Cabal doesn't
  pure (ident, BlobKey sha (FileSize size'))

splitColon :: Text -> Maybe (Text, Text)
splitColon t' =
  let (x, y) = T.break (== ':') t'
  in  (x, ) <$> T.stripPrefix ":" y

-- | Parse a 'PackageIdentifierRevision'
--
-- @since 0.1.0.0
parsePackageIdentifierRevision ::
     Text
  -> Either PantryException PackageIdentifierRevision
parsePackageIdentifierRevision t =
  maybe (Left $ PackageIdentifierRevisionParseFail t) Right $ do
    let (identT, cfiT) = T.break (== '@') t
    PackageIdentifier name version <- parsePackageIdentifier $ T.unpack identT
    cfi <-
      case splitColon cfiT of
        Just ("@sha256", shaSizeT) -> do
          let (shaT, sizeT) = T.break (== ',') shaSizeT
          sha <- either (const Nothing) Just $ SHA256.fromHexText shaT
          msize <-
            case T.stripPrefix "," sizeT of
              Nothing -> Just Nothing
              Just sizeT' ->
                case decimal sizeT' of
                  Right (size', "") -> Just $ Just $ FileSize size'
                  _ -> Nothing
          pure $ CFIHash sha msize
        Just ("@rev", revT) ->
          case decimal revT of
            Right (rev, "") -> pure $ CFIRevision $ Revision rev
            _ -> Nothing
        Nothing -> pure CFILatest
        _ -> Nothing
    pure $ PackageIdentifierRevision name version cfi

data Mismatch a = Mismatch
  { mismatchExpected :: !a
  , mismatchActual :: !a
  }

-- | Things that can go wrong in pantry. Note two things:
--
-- * Many other exception types may be thrown from underlying libraries. Pantry
--   does not attempt to wrap these underlying exceptions.
--
-- * We may add more constructors to this data type in minor version bumps of
--   pantry. This technically breaks the PVP. You should not be writing pattern
--   matches against this type that expect total matching.
--
-- @since 0.1.0.0
data PantryException
  = PackageIdentifierRevisionParseFail !Text
  | InvalidCabalFile
      !(Either RawPackageLocationImmutable (Path Abs File))
      !(Maybe Version)
      ![PError]
      ![PWarning]
  | TreeWithoutCabalFile !RawPackageLocationImmutable
  | TreeWithMultipleCabalFiles !RawPackageLocationImmutable ![SafeFilePath]
  | MismatchedCabalName !(Path Abs File) !PackageName
  | NoLocalPackageDirFound !(Path Abs Dir)
  | NoCabalFileFound !(Path Abs Dir)
  | MultipleCabalFilesFound !(Path Abs Dir) ![Path Abs File]
  | InvalidWantedCompiler !Text
  | InvalidSnapshotLocation !(Path Abs Dir) !Text
  | InvalidOverrideCompiler !WantedCompiler !WantedCompiler
  | InvalidFilePathSnapshot !Text
  | InvalidSnapshot !RawSnapshotLocation !SomeException
  | MismatchedPackageMetadata
      !RawPackageLocationImmutable
      !RawPackageMetadata
      !(Maybe TreeKey)
      !PackageIdentifier
  | Non200ResponseStatus !Status
  | InvalidBlobKey !(Mismatch BlobKey)
  | Couldn'tParseSnapshot !RawSnapshotLocation !String
  | WrongCabalFileName !RawPackageLocationImmutable !SafeFilePath !PackageName
  | DownloadInvalidSHA256 !Text !(Mismatch SHA256)
  | DownloadInvalidSize !Text !(Mismatch FileSize)
  | DownloadTooLarge !Text !(Mismatch FileSize)
  -- ^ Different from 'DownloadInvalidSize' since 'mismatchActual' is
  -- a lower bound on the size from the server.
  | LocalInvalidSHA256 !(Path Abs File) !(Mismatch SHA256)
  | LocalInvalidSize !(Path Abs File) !(Mismatch FileSize)
  | UnknownArchiveType !ArchiveLocation
  | InvalidTarFileType !ArchiveLocation !FilePath !Tar.FileType
  | UnsupportedTarball !ArchiveLocation !Text
  | NoHackageCryptographicHash !PackageIdentifier
  | FailedToCloneRepo !SimpleRepo
  | TreeReferencesMissingBlob !RawPackageLocationImmutable !SafeFilePath !BlobKey
  | CompletePackageMetadataMismatch !RawPackageLocationImmutable !PackageMetadata
  | CRC32Mismatch !ArchiveLocation !FilePath !(Mismatch Word32)
  | UnknownHackagePackage !PackageIdentifierRevision !FuzzyResults
  | CannotCompleteRepoNonSHA1 !Repo
  | MutablePackageLocationFromUrl !Text
  | MismatchedCabalFileForHackage !PackageIdentifierRevision !(Mismatch PackageIdentifier)
  | PackageNameParseFail !Text
  | PackageVersionParseFail !Text
  | InvalidCabalFilePath !(Path Abs File)
  | DuplicatePackageNames !Utf8Builder ![(PackageName, [RawPackageLocationImmutable])]
  | MigrationFailure !Text !(Path Abs File) !SomeException
  | NoCasaConfig
  | InvalidTreeFromCasa !BlobKey !ByteString
  | ParseSnapNameException !Text
  | HpackLibraryException !(Path Abs File) !String
  | HpackExeException !FilePath !(Path Abs Dir) !SomeException
  deriving Typeable

instance Exception PantryException where

instance Show PantryException where
  show = T.unpack . utf8BuilderToText . display

-- To support the Haskell Foundation's
-- [Haskell Error Index](https://errors.haskell.org/) initiative, all Pantry
-- error messages generated by Pantry itself begin with an unique code in the
-- form `[S-nnn]`, where `nnn` is a three-digit number in the range 100 to 999.
-- The numbers are selected at random, not in sequence.
--
-- Prettier versions of these error messages are also provided. See the instance
-- of Pretty.
instance Display PantryException where
  display NoCasaConfig =
    "Error: [S-889]\n"
    <> "The Pantry configuration has no Casa configuration."
  display (InvalidTreeFromCasa blobKey _bs) =
    "Error: [S-258]\n"
    <> "Invalid tree from casa: "
    <> display blobKey
  display (PackageIdentifierRevisionParseFail text) =
    "Error: [S-360]\n"
    <> "Invalid package identifier (with optional revision): "
    <> display text
  display (InvalidCabalFile loc mversion errs warnings) =
    "Error: [S-242]\n"
    <> "Unable to parse cabal file from package "
    <> either display (fromString . toFilePath) loc
    <> "\n\n"
    <> foldMap
         ( \(PError pos msg) ->
                 "- "
              <> fromString (showPos pos)
              <> ": "
              <> fromString msg
              <> "\n"
         )
         errs
    <> foldMap
         ( \(PWarning _ pos msg) ->
                 "- "
              <> fromString (showPos pos)
              <> ": "
              <> fromString msg
              <> "\n"
         )
         warnings
    <> ( case mversion of
           Just version
             | version > cabalSpecLatestVersion ->
                    "\n\nThe cabal file uses the cabal specification version "
                 <> fromString (versionString version)
                 <> ", but we only support up to version "
                 <> fromString (versionString cabalSpecLatestVersion)
                 <> ".\nRecommended action: upgrade your build tool (e.g., `stack upgrade`)."
           _ -> mempty
       )
  display (TreeWithoutCabalFile pl) =
    "Error: [S-654]\n"
    <> "No cabal file found for "
    <> display pl
  display (TreeWithMultipleCabalFiles pl sfps) =
    "Error: [S-500]\n"
    <> "Multiple cabal files found for "
    <> display pl
    <> ": "
    <> fold (intersperse ", " (map display sfps))
  display (MismatchedCabalName fp name) =
    "Error: [S-910]\n"
    <> "The Cabal file:\n"
    <> fromString (toFilePath fp)
    <> "\nis not named after the package that it defines.\n"
    <> "Please rename the file to: "
    <> fromString (packageNameString name)
    <> ".cabal\n"
    <> "Hackage rejects packages where the first part of the Cabal file name "
    <> "is not the package name."
  display (NoLocalPackageDirFound dir) =
    "Error: [S-395]\n"
    <> "Stack looks for packages in the directories configured in\n"
    <> "the 'packages' and 'extra-deps' fields defined in your stack.yaml\n"
    <> "The current entry points to "
    <> fromString (toFilePath dir)
    <> ",\nbut no such directory could be found. If, alternatively, a package\n"
    <> "in the package index was intended, its name and version must be\n"
    <> "specified as an extra-dep."
  display (NoCabalFileFound dir) =
    "Error: [S-636]\n"
    <> "Stack looks for packages in the directories configured in\n"
    <> "the 'packages' and 'extra-deps' fields defined in your stack.yaml\n"
    <> "The current entry points to "
    <> fromString (toFilePath dir)
    <> ",\nbut no .cabal or package.yaml file could be found there."
  display (MultipleCabalFilesFound dir files) =
    "Error: [S-368]\n"
    <> "Multiple .cabal files found in directory "
    <> fromString (toFilePath dir)
    <> ":\n"
    <> fold
         ( intersperse
             "\n"
             (map (\x -> "- " <> fromString (toFilePath (filename x))) files)
         )
  display (InvalidWantedCompiler t) =
    "Error: [S-204]\n"
    <> "Invalid wanted compiler: "
    <> display t
  display (InvalidSnapshotLocation dir t) =
    "Error: [S-935]\n"
    <> "Invalid snapshot location "
    <> displayShow t
    <> " relative to directory "
    <> displayShow (toFilePath dir)
  display (InvalidOverrideCompiler x y) =
    "Error: [S-287]\n"
    <> "Specified compiler for a resolver ("
    <> display x
    <> "), but also specified an override compiler ("
    <> display y
    <> ")"
  display (InvalidFilePathSnapshot t) =
    "Error: [S-617]\n"
    <> "Specified snapshot as file path with "
    <> displayShow t
    <> ", but not reading from a local file"
  display (InvalidSnapshot loc err) =
    "Error: [S-775]\n"
    <> "Exception while reading snapshot from "
    <> display loc
    <> ":\n"
    <> displayShow err
  display (MismatchedPackageMetadata loc pm mtreeKey foundIdent) =
    "Error: [S-427]\n"
    <> "Mismatched package metadata for "
    <> display loc
    <> "\nFound: "
    <> fromString (packageIdentifierString foundIdent)
    <> ( case mtreeKey of
           Nothing -> mempty
           Just treeKey -> " with tree " <> display treeKey
       )
    <> "\nExpected: "
    <> display pm
  display (Non200ResponseStatus status) =
    "Error: [S-571]\n"
    <> "Unexpected non-200 HTTP status code: "
    <> displayShow (statusCode status)
  display (InvalidBlobKey Mismatch{..}) =
    "Error: [S-236]\n"
    <> "Invalid blob key found, expected: "
    <> display mismatchExpected
    <> ", actual: "
    <> display mismatchActual
  display (Couldn'tParseSnapshot sl err) =
    "Error: [S-645]\n"
    <> "Couldn't parse snapshot from "
    <> display sl
    <> ": "
    <> fromString err
  display (WrongCabalFileName pl sfp name) =
    "Error: [S-575]\n"
    <> "Wrong cabal file name for package "
    <> display pl
    <> "\nThe cabal file is named "
    <> display sfp
    <> ", but package name is "
    <> fromString (packageNameString name)
    <> "\nFor more information, see:\n  - https://github.com/commercialhaskell/stack/issues/317\n  -https://github.com/commercialhaskell/stack/issues/895"
  display (DownloadInvalidSHA256 url Mismatch {..}) =
    "Error: [S-394]\n"
    <> "Mismatched SHA256 hash from "
    <> display url
    <> "\nExpected: "
    <> display mismatchExpected
    <> "\nActual:   "
    <> display mismatchActual
  display (DownloadInvalidSize url Mismatch {..}) =
    "Error: [S-401]\n"
    <> "Mismatched download size from "
    <> display url
    <> "\nExpected: "
    <> display mismatchExpected
    <> "\nActual:   "
    <> display mismatchActual
  display (DownloadTooLarge url Mismatch {..}) =
    "Error: [S-113]\n"
    <> "Download from "
    <> display url
    <> " was too large.\n"
    <> "Expected: "
    <> display mismatchExpected
    <> ", stopped after receiving: "
    <> display mismatchActual
  display (LocalInvalidSHA256 path Mismatch {..}) =
    "Error: [S-834]\n"
    <> "Mismatched SHA256 hash from "
    <> fromString (toFilePath path)
    <> "\nExpected: "
    <> display mismatchExpected
    <> "\nActual:   "
    <> display mismatchActual
  display (LocalInvalidSize path Mismatch {..}) =
    "Error: [S-713]\n"
    <> "Mismatched file size from "
    <> fromString (toFilePath path)
    <> "\nExpected: "
    <> display mismatchExpected
    <> "\nActual:   "
    <> display mismatchActual
  display (UnknownArchiveType loc) =
    "Error: [S-372]\n"
    <> "Unable to determine archive type of: "
    <> display loc
  display (InvalidTarFileType loc fp x) =
    "Error: [S-950]\n"
    <> "Unsupported tar file type in archive "
    <> display loc
    <> " at file "
    <> fromString fp
    <> ": "
    <> displayShow x
  display (UnsupportedTarball loc err) =
    "Error: [S-760]\n"
    <> "Unsupported tarball from "
    <> display loc
    <> ": "
    <> display err
  display (NoHackageCryptographicHash ident) =
    "Error: [S-922]\n"
    <> "No cryptographic hash found for Hackage package "
    <> fromString (packageIdentifierString ident)
  display (FailedToCloneRepo repo) =
    "Error: [S-109]\n"
    <> "Failed to clone repo "
    <> display repo
  display (TreeReferencesMissingBlob loc sfp key) =
    "Error: [S-237]\n"
    <> "The package "
    <> display loc
    <> " needs blob "
    <> display key
    <> " for file path "
    <> display sfp
    <> ", but the blob is not available"
  display (CompletePackageMetadataMismatch loc pm) =
    "Error: [S-984]\n"
    <> "When completing package metadata for "
    <> display loc
    <> ", some values changed in the new package metadata: "
    <> display pm
  display (CRC32Mismatch loc fp Mismatch {..}) =
    "Error: [S-607]\n"
    <> "CRC32 mismatch in ZIP file from "
    <> display loc
    <> " on internal file "
    <> fromString fp
    <> "\nExpected: "
    <> display mismatchExpected
    <> "\nActual:   "
    <> display mismatchActual
  display (UnknownHackagePackage pir fuzzy) =
    "Error: [S-476]\n"
    <> "Could not find "
    <> display pir
    <> " on Hackage"
    <> displayFuzzy fuzzy
  display (CannotCompleteRepoNonSHA1 repo) =
    "Error: [S-112]\n"
    <> "Cannot complete repo information for a non SHA1 commit due to non-reproducibility: "
    <> display repo
  display (MutablePackageLocationFromUrl t) =
    "Error: [S-321]\n"
    <> "Cannot refer to a mutable package location from a URL: "
    <> display t
  display (MismatchedCabalFileForHackage pir Mismatch{..}) =
    "Error: [S-377]\n"
    <> "When processing cabal file for Hackage package "
    <> display pir
    <> ":\nMismatched package identifier."
    <> "\nExpected: "
    <> fromString (packageIdentifierString mismatchExpected)
    <> "\nActual:   "
    <> fromString (packageIdentifierString mismatchActual)
  display (PackageNameParseFail t) =
    "Error: [S-580]\n"
    <> "Invalid package name: "
    <> display t
  display (PackageVersionParseFail t) =
    "Error: [S-479]\n"
    <> "Invalid version: "
    <> display t
  display (InvalidCabalFilePath fp) =
    "Error: [S-824]\n"
    <> "File path contains a name which is not a valid package name: "
    <> fromString (toFilePath fp)
  display (DuplicatePackageNames source pairs') =
    "Error: [S-674]\n"
    <> "Duplicate package names ("
    <> source
    <> "):\n"
    <> foldMap
         ( \(name, locs) ->
                fromString (packageNameString name)
             <> ":\n"
             <> foldMap (\loc -> "- " <> display loc <> "\n") locs
         )
         pairs'
  display (MigrationFailure desc fp err) =
    "Error: [S-536]\n"
    <> "Encountered error while migrating database "
    <> display desc
    <> "\nlocated at "
    <> fromString (toFilePath fp)
    <> ":"
    <> "\n    "
    <> displayShow err
  display (ParseSnapNameException t) =
    "Error: [S-994]\n"
     <> "Invalid snapshot name: "
     <> display t
  display (HpackLibraryException file err) =
    "Error: [S-305]\n"
    <> "Failed to generate a Cabal file using the Hpack library on file:\n"
    <> fromString (toFilePath file)
    <> "\n\n"
    <> "The error encountered was:\n\n"
    <> fromString err
  display (HpackExeException fp dir err) =
    "Error: [S-720]\n"
    <> "Failed to generate a Cabal file using the Hpack executable:\n"
    <> fromString fp
    <> "in directory: "
    <> fromString (toFilePath dir)
    <> "\n\n"
    <> "The error encountered was:\n\n"
    <> fromString (show err)

-- See also the instance of Display. Although prettier, these messages are
-- intended to be substantively the same as the corresponding 'black and white'
-- versions.
instance Pretty PantryException where
  pretty NoCasaConfig =
    "[S-889]"
    <> line
    <> flow "The Pantry configuration has no Casa configuration."
  pretty (InvalidTreeFromCasa blobKey _bs) =
    "[S-258]"
    <> line
    <> fillSep
         [ flow "Invalid tree from casa:"
         , fromString . T.unpack $ textDisplay blobKey
         ]
  pretty (PackageIdentifierRevisionParseFail text) =
    "[S-360]"
    <> line
    <> fillSep
         [ flow "Invalid package identifier (with optional revision):"
         , fromString $ T.unpack text
         ]
  pretty (InvalidCabalFile loc mversion errs warnings) =
    "[S-242]"
    <> line
    <> fillSep
         [ flow "Unable to parse Cabal file from package"
         , either pretty pretty loc <> ":"
         ]
    <> line
    <> bulletedList
         ( map (\(PError pos msg) -> fillSep
             [ fromString (showPos pos) <> ":"
             , fromString msg
             ])
             errs
         )
    <> line
    <> bulletedList
         ( map (\(PWarning _ pos msg) -> fillSep
             [ fromString (showPos pos) <> ":"
             , fromString msg
             ])
             warnings
         )
    <> ( case mversion of
           Just version | version > cabalSpecLatestVersion ->
                line
             <> fillSep
                  [ flow "The Cabal file uses the Cabal specification version"
                  , style Current (fromString $ versionString version) <> ","
                  , flow "but we only support up to version"
                  , fromString (versionString cabalSpecLatestVersion) <> "."
                  , flow "Recommended action: upgrade your build tool"
                  , parens (fillSep
                      [ "e.g."
                      , style Shell (flow "stack upgrade")
                      ]) <> "."
                  ]
           _ -> mempty
       )
  pretty (TreeWithoutCabalFile loc) =
    "[S-654]"
    <> line
    <> fillSep
         [ flow "No Cabal file found for"
         , pretty loc <> "."
         ]
  pretty (TreeWithMultipleCabalFiles loc sfps) =
    "[S-500]"
    <> line
    <> fillSep
         ( flow "Multiple Cabal files found for"
         : (pretty loc <> ":")
         : mkNarrativeList (Just File) False
             (map (fromString . T.unpack . textDisplay) sfps :: [StyleDoc])
         )
  pretty (MismatchedCabalName fp name) =
    "[S-910]"
    <> line
    <> fillSep
         [ flow "The Cabal file"
         , pretty fp
         , flow "is not named after the package that it defines. Please rename"
         , flow "the file to"
         , style File (fromString $ packageNameString name <> ".cabal") <> "."
         , flow "Hackage rejects packages where the first part of the Cabal"
         , flow "file name is not the package name."
         ]
  pretty (NoLocalPackageDirFound dir) =
    "[S-395]"
    <> line
    <> fillSep
         [ flow "Stack looks for packages in the directories configured in the"
         , style Shell "packages"
         , "and"
         , style Shell "extra-deps"
         , flow "fields defined in your"
         , style File "stack.yaml" <> "."
         , flow "The current entry points to"
         , pretty dir
         , flow "but no such directory could be found. If, alternatively, a"
         , flow "package in the package index was intended, its name and"
         , flow "version must be specified as an extra-dep."
         ]
  pretty (NoCabalFileFound dir) =
    "[S-636]"
    <> line
    <> fillSep
         [ flow "Stack looks for packages in the directories configured in the"
         , style Shell "packages"
         , "and"
         , style Shell "extra-deps"
         , flow "fields defined in your"
         , style File "stack.yaml" <> "."
         , flow "The current entry points to"
         , pretty dir
         , flow "but no Cabal file or"
         , style File "package.yaml"
         , flow "could be found there."
         ]
  pretty (MultipleCabalFilesFound dir files) =
    "[S-368]"
    <> line
    <> fillSep
         ( flow "Multiple Cabal files found in directory"
         : (pretty dir <> ":")
         : mkNarrativeList (Just File) False
             (map (pretty . filename) files)
         )
  pretty (InvalidWantedCompiler t) =
    "[S-204]"
    <> line
    <> fillSep
         [ flow "Invalid wanted compiler:"
         , style Current (fromString $ T.unpack t) <> "."
         ]
  pretty (InvalidSnapshotLocation dir t) =
    "[S-935]"
    <> line
    <> fillSep
         [ flow "Invalid snapshot location"
         , style Current (fromString $ T.unpack t)
         , flow "relative to directory"
         , pretty dir <> "."
         ]
  pretty (InvalidOverrideCompiler x y) =
    "[S-287]"
    <> line
    <> fillSep
         [ flow "Specified compiler for a resolver"
         , parens (style Shell (fromString . T.unpack $ textDisplay x))
         , flow "but also specified an override compiler"
         , parens (style Shell (fromString . T.unpack $ textDisplay y)) <> "."
         ]
  pretty (InvalidFilePathSnapshot t) =
    "[S-617]"
    <> line
    <> fillSep
         [ flow "Specified snapshot as file path with"
         , style File (fromString $ T.unpack t) <> ","
         , flow "but not reading from a local file."
         ]
  pretty (InvalidSnapshot loc err) =
    "[S-775]"
    <> line
    <> fillSep
         [ flow "Exception while reading snapshot from"
         , pretty loc <> ":"
         ]
    <> blankLine
    <> string (displayException err)
  pretty (MismatchedPackageMetadata loc pm mtreeKey foundIdent) =
    "[S-427]"
    <> line
    <> fillSep
         [ flow "Mismatched package metadata for"
         , pretty loc <> "."
         ]
    <> blankLine
    <> hang 10 (fillSep
         [ "Expected:"
         , let t = textDisplay pm
           in  if T.null t
                 then "nothing."
                 else fromString $ T.unpack t <> "."
         ])
    <> line
    <> hang 10 (fillSep
         [ "Found:   "
         , fromString $ packageIdentifierString foundIdent <> case mtreeKey of
             Nothing -> "."
             _ -> mempty
         , case mtreeKey of
             Nothing -> mempty
             Just treeKey -> fillSep
               [ "with tree"
               , fromString . T.unpack $ textDisplay treeKey <> "."
               ]
         ])
  pretty (Non200ResponseStatus status) =
    "[S-571]"
    <> line
    <> fillSep
         [ flow "Unexpected non-200 HTTP status code:"
         , (fromString . show $ statusCode status) <> "."
         ]
  pretty (InvalidBlobKey Mismatch{..}) =
    "[S-236]"
    <> line
    <> fillSep
         [ flow "Invalid blob key found, expected:"
         , fromString . T.unpack $ textDisplay mismatchExpected <> ","
         , "actual:"
         , fromString . T.unpack $ textDisplay mismatchActual <> "."
         ]
  pretty (Couldn'tParseSnapshot sl err) =
    "[S-645]"
    <> line
    <> fillSep
         [ flow "Couldn't parse snapshot from"
         , pretty sl <> ":"
         ]
    <> blankLine
    <> string err
  pretty (WrongCabalFileName loc sfp name) =
    "[S-575]"
    <> line
    <> fillSep
         [ flow "Wrong Cabal file name for package"
         , pretty loc <> "."
         , flow "The Cabal file is named"
         , style File (fromString . T.unpack $ textDisplay sfp) <> ","
         , flow "but package name is"
         , fromString (packageNameString name) <> "."
         , flow "For more information, see"
         , style Url "https://github.com/commercialhaskell/stack/issues/317"
         , "and"
         , style Url "https://github.com/commercialhaskell/stack/issues/895" <> "."
         ]
  pretty (DownloadInvalidSHA256 url Mismatch {..}) =
    "[S-394]"
    <> line
    <> fillSep
         [ flow "Mismatched SHA256 hash from"
         , style Url (fromString $ T.unpack url) <> "."
         ]
    <> blankLine
    <> hang 10 (fillSep
         [ "Expected:"
         , fromString . T.unpack $ textDisplay mismatchExpected <> "."
         ])
    <> line
    <> hang 10 (fillSep
         [ "Actual:  "
         , fromString . T.unpack $ textDisplay mismatchActual <> "."
         ])
  pretty (DownloadInvalidSize url Mismatch {..}) =
    "[S-401]"
    <> line
    <> fillSep
         [ flow "Mismatched download size from"
         , style Url (fromString $ T.unpack url) <> "."
         ]
    <> blankLine
    <> hang 10 (fillSep
         [ "Expected:"
         , fromString . T.unpack $ textDisplay mismatchExpected <> "."
         ])
    <> line
    <> hang 10 (fillSep
         [ "Actual:  "
         , fromString . T.unpack $ textDisplay mismatchActual <> "."
         ])
  pretty (DownloadTooLarge url Mismatch {..}) =
    "[S-113]"
    <> line
    <> fillSep
         [ flow "Download from"
         , style Url (fromString $ T.unpack url)
         , flow "was too large. Expected:"
         , fromString . T.unpack $ textDisplay mismatchExpected <> ","
         , flow "stopped after receiving:"
         , fromString . T.unpack $ textDisplay mismatchActual <> "."
         ]
  pretty (LocalInvalidSHA256 path Mismatch {..}) =
    "[S-834]"
    <> line
    <> fillSep
         [ flow "Mismatched SHA256 hash from"
         , pretty path <> "."
         ]
    <> blankLine
    <> hang 10 (fillSep
         [ "Expected:"
         , fromString . T.unpack $ textDisplay mismatchExpected <> "."
         ])
    <> line
    <> hang 10 (fillSep
         [ "Actual:  "
         , fromString . T.unpack $ textDisplay mismatchActual <> "."
         ])
  pretty (LocalInvalidSize path Mismatch {..}) =
    "[S-713]"
    <> line
    <> fillSep
         [ flow "Mismatched file size from"
         , pretty path <> "."
         ]
    <> blankLine
    <> hang 10 (fillSep
         [ "Expected:"
         , fromString . T.unpack $ textDisplay mismatchExpected <> "."
         ])
    <> line
    <> hang 10 (fillSep
         [ "Actual:  "
         , fromString . T.unpack $ textDisplay mismatchActual <> "."
         ])
  pretty (UnknownArchiveType loc) =
    "[S-372]"
    <> line
    <> fillSep
         [ flow "Unable to determine archive type of:"
         , pretty loc <> "."
         ]
  pretty (InvalidTarFileType loc fp x) =
    "[S-950]"
    <> line
    <> fillSep
         [ flow "Unsupported tar file type in archive"
         , pretty loc
         , flow "at file"
         , style File (fromString fp) <> ":"
         , fromString $ show x <> "."
         ]
  pretty (UnsupportedTarball loc err) =
    "[S-760]"
    <> line
    <> fillSep
         [ flow "Unsupported tarball from"
         , pretty loc <> ":"
         ]
    <> blankLine
    <> string (T.unpack err)
  pretty (NoHackageCryptographicHash ident) =
    "[S-922]"
    <> line
    <> fillSep
         [ flow "No cryptographic hash found for Hackage package"
         , fromString (packageIdentifierString ident) <> "."
         ]
  pretty (FailedToCloneRepo repo) =
    "[S-109]"
    <> line
    <> fillSep
         [ flow "Failed to clone repository"
         , fromString . T.unpack $ textDisplay repo
         ]
  pretty (TreeReferencesMissingBlob loc sfp key) =
    "[S-237]"
    <> line
    <> fillSep
         [ flow "The package"
         , pretty loc
         , flow "needs blob"
         , fromString . T.unpack $ textDisplay key
         , flow "for file path"
         , style File (fromString . T.unpack $ textDisplay sfp) <> ","
         , flow "but the blob is not available."
         ]
  pretty (CompletePackageMetadataMismatch loc pm) =
    "[S-984]"
    <> line
    <> fillSep
         [ flow "When completing package metadata for"
         , pretty loc <> ","
         , flow "some values changed in the new package metadata:"
         , fromString . T.unpack $ textDisplay pm <> "."
         ]
  pretty (CRC32Mismatch loc fp Mismatch {..}) =
    "[S-607]"
    <> line
    <> fillSep
         [ flow "CRC32 mismatch in Zip file from"
         , pretty loc
         , flow "on internal file"
         , style File (fromString fp)
         ]
    <> blankLine
    <> hang 10 (fillSep
         [ "Expected:"
         , fromString . T.unpack $ textDisplay mismatchExpected <> "."
         ])
    <> line
    <> hang 10 (fillSep
         [ "Actual:  "
         , fromString . T.unpack $ textDisplay mismatchActual <> "."
         ])
  pretty (UnknownHackagePackage pir fuzzy) =
    "[S-476]"
    <> line
    <> fillSep
         [ flow "Could not find"
         , style Error (fromString . T.unpack $ textDisplay pir)
         , flow "on Hackage."
         ]
    <> prettyFuzzy fuzzy
  pretty (CannotCompleteRepoNonSHA1 repo) =
    "[S-112]"
    <> line
    <> fillSep
         [ flow "Cannot complete repo information for a non SHA1 commit due to"
         , "non-reproducibility:"
         , fromString . T.unpack $ textDisplay repo <> "."
         ]
  pretty (MutablePackageLocationFromUrl t) =
    "[S-321]"
    <> line
    <> fillSep
         [ flow "Cannot refer to a mutable package location from a URL:"
         , style Url (fromString $ T.unpack t) <> "."
         ]
  pretty (MismatchedCabalFileForHackage pir Mismatch{..}) =
    "[S-377]"
    <> line
    <> fillSep
         [ flow "When processing Cabal file for Hackage package"
         , fromString . T.unpack $ textDisplay pir <> ","
         , flow "mismatched package identifier."
         ]
    <> blankLine
    <> hang 10 (fillSep
         [ "Expected:"
         , fromString (packageIdentifierString mismatchExpected) <> "."
         ])
    <> line
    <> hang 10 (fillSep
         [ "Actual:  "
         , fromString (packageIdentifierString mismatchActual) <> "."
         ])
  pretty (PackageNameParseFail t) =
    "[S-580]"
    <> line
    <> fillSep
         [ flow "Invalid package name:"
         , fromString $ T.unpack t <> "."
         ]
  pretty (PackageVersionParseFail t) =
    "[S-479]"
    <> line
    <> fillSep
         [ flow "Invalid version:"
         , fromString $ T.unpack t <> "."
         ]
  pretty (InvalidCabalFilePath fp) =
    "[S-824]"
    <> line
    <> fillSep
         [ flow "File path contains a name which is not a valid package name:"
         , pretty fp <> "."
         ]
  pretty (DuplicatePackageNames source pairs') =
    "[S-674]"
    <> line
    <> fillSep
         [ flow "Duplicate package names"
         , parens (fromString . T.unpack $ textDisplay source) <> ":"
         ]
    <> line
    <> foldMap
         ( \(name, locs) ->
                fromString (packageNameString name) <> ":"
             <> line
             <> bulletedList (map pretty locs)
             <> line
         )
         pairs'
  pretty (MigrationFailure desc fp err) =
    "[S-536]"
    <> line
    <> fillSep
         [ flow "Encountered error while migrating database"
         , fromString $ T.unpack desc
         , flow "located at"
         , pretty fp <> ":"
         ]
    <> blankLine
    <> string (displayException err)
  pretty (ParseSnapNameException t) =
    "[S-994]"
    <> line
    <> fillSep
         [ flow "Invalid snapshot name:"
         , fromString $ T.unpack t <> "."
         ]
  pretty (HpackLibraryException file err) =
    "[S-305]"
    <> line
    <> fillSep
         [ flow "Failed to generate a Cabal file using the Hpack library on"
         , "file:"
         , pretty file <> "."
         , flow "The error encountered was:"
         ]
    <> blankLine
    <> string err
  pretty (HpackExeException fp dir err) =
    "[S-720]"
    <> line
    <> fillSep
         [ flow "Failed to generate a Cabal file using the Hpack executable:"
         , style File (fromString fp)
         , flow "in directory:"
         , pretty dir <> "."
         , flow "The error encountered was:"
         ]
    <> blankLine
    <> string (displayException err)

blankLine :: StyleDoc
blankLine = line <> line

data FuzzyResults
  = FRNameNotFound ![PackageName]
  | FRVersionNotFound !(NonEmpty PackageIdentifierRevision)
  | FRRevisionNotFound !(NonEmpty PackageIdentifierRevision)

displayFuzzy :: FuzzyResults -> Utf8Builder
displayFuzzy (FRNameNotFound names) =
  case NE.nonEmpty names of
    Nothing -> ""
    Just names' ->
      "\nPerhaps you meant " <>
      orSeparated (NE.map (fromString . packageNameString) names') <>
      "?"
displayFuzzy (FRVersionNotFound pirs) =
  "\nPossible candidates: " <>
  commaSeparated (NE.map display pirs) <>
  "."
displayFuzzy (FRRevisionNotFound pirs) =
  "\nThe specified revision was not found.\nPossible candidates: " <>
  commaSeparated (NE.map display pirs) <>
  "."

prettyFuzzy :: FuzzyResults -> StyleDoc
prettyFuzzy (FRNameNotFound names) =
  case NE.nonEmpty names of
    Nothing -> mempty
    Just names' ->
         line
      <> fillSep
           ( flow "Perhaps you meant one of"
           : mkNarrativeList Nothing False
               (NE.toList $ NE.map (fromString . packageNameString) names' :: [StyleDoc])
           )
prettyFuzzy (FRVersionNotFound pirs) =
     line
  <> fillSep
       ( flow "Possible candidates:"
       : mkNarrativeList Nothing False
           (NE.toList $ NE.map (fromString . T.unpack . textDisplay) pirs :: [StyleDoc])
       )
prettyFuzzy (FRRevisionNotFound pirs) =
     line
  <> fillSep
       ( flow "The specified revision was not found. Possible candidates:"
       : mkNarrativeList Nothing False
           (NE.toList $ NE.map (fromString . T.unpack . textDisplay) pirs :: [StyleDoc])
       )

orSeparated :: NonEmpty Utf8Builder -> Utf8Builder
orSeparated xs
  | NE.length xs == 1 = NE.head xs
  | NE.length xs == 2 = NE.head xs <> " or " <> NE.last xs
  | otherwise = fold (intersperse ", " (NE.init xs)) <> ", or " <> NE.last xs

commaSeparated :: NonEmpty Utf8Builder -> Utf8Builder
commaSeparated = fold . NE.intersperse ", "

cabalSpecLatestVersion :: Version
cabalSpecLatestVersion = mkVersion $ cabalSpecToVersionDigits cabalSpecLatest

#if !MIN_VERSION_Cabal(3,4,0)
cabalSpecToVersionDigits :: CabalSpecVersion -> [Int]
cabalSpecToVersionDigits CabalSpecV3_0   = [3,0]
cabalSpecToVersionDigits CabalSpecV2_4   = [2,4]
cabalSpecToVersionDigits CabalSpecV2_2   = [2,2]
cabalSpecToVersionDigits CabalSpecV2_0   = [2,0]
cabalSpecToVersionDigits CabalSpecV1_24  = [1,24]
cabalSpecToVersionDigits CabalSpecV1_22  = [1,22]
cabalSpecToVersionDigits CabalSpecV1_20  = [1,20]
cabalSpecToVersionDigits CabalSpecV1_18  = [1,18]
cabalSpecToVersionDigits CabalSpecV1_12  = [1,12]
cabalSpecToVersionDigits CabalSpecV1_10  = [1,10]
cabalSpecToVersionDigits CabalSpecV1_8   = [1,8]
cabalSpecToVersionDigits CabalSpecV1_6   = [1,6]
cabalSpecToVersionDigits CabalSpecV1_4   = [1,4]
cabalSpecToVersionDigits CabalSpecV1_2   = [1,2]
cabalSpecToVersionDigits CabalSpecV1_0   = [1,0]
#endif

data BuildFile
  = BFCabal !SafeFilePath !TreeEntry
  | BFHpack !TreeEntry
    -- We don't need SafeFilePath for Hpack since it has to be package.yaml file
  deriving (Show, Eq)

data FileType = FTNormal | FTExecutable
  deriving (Show, Eq, Enum, Bounded, Ord)

instance PersistField FileType where
  toPersistValue FTNormal = PersistInt64 1
  toPersistValue FTExecutable = PersistInt64 2

  fromPersistValue v = do
    i <- fromPersistValue v
    case i :: Int64 of
      1 -> Right FTNormal
      2 -> Right FTExecutable
      _ -> Left $ "Invalid FileType: " <> tshow i

instance PersistFieldSql FileType where
  sqlType _ = SqlInt32

data TreeEntry = TreeEntry
  { teBlob :: !BlobKey
  , teType :: !FileType
  }
  deriving (Show, Eq, Ord)

newtype SafeFilePath = SafeFilePath Text
  deriving (Show, Eq, Ord, Display)

instance PersistField SafeFilePath where
  toPersistValue = toPersistValue . unSafeFilePath
  fromPersistValue v = do
    t <- fromPersistValue v
    maybe (Left $ "Invalid SafeFilePath: " <> t) Right $ mkSafeFilePath t

instance PersistFieldSql SafeFilePath where
  sqlType _ = SqlString

unSafeFilePath :: SafeFilePath -> Text
unSafeFilePath (SafeFilePath t) = t

safeFilePathToPath :: (MonadThrow m) => Path Abs Dir -> SafeFilePath -> m (Path Abs File)
safeFilePathToPath dir (SafeFilePath path) = do
  fpath <- parseRelFile (T.unpack path)
  pure $ dir </> fpath

mkSafeFilePath :: Text -> Maybe SafeFilePath
mkSafeFilePath t = do
  guard $ not $ "\\" `T.isInfixOf` t
  guard $ not $ "//" `T.isInfixOf` t
  guard $ not $ "\n" `T.isInfixOf` t
  guard $ not $ "\0" `T.isInfixOf` t

  (c, _) <- T.uncons t
  guard $ c /= '/'

  guard $ (not . any (T.all (== '.'))) $ T.split (== '/') t

  Just $ SafeFilePath t

-- | SafeFilePath for `package.yaml` file.
hpackSafeFilePath :: SafeFilePath
hpackSafeFilePath =
    let fpath = mkSafeFilePath (T.pack Hpack.packageConfig)
    in case fpath of
         Nothing -> error $
           "hpackSafeFilePath: Not able to encode " <> Hpack.packageConfig
         Just sfp -> sfp

-- | The hash of the binary representation of a 'Tree'.
--
-- @since 0.1.0.0
newtype TreeKey = TreeKey BlobKey
  deriving (Show, Eq, Ord, Generic, Typeable, ToJSON, FromJSON, NFData, Display)

-- | Represents the contents of a tree, which is a mapping from
-- relative file paths to 'TreeEntry's.
--
-- @since 0.1.0.0
newtype Tree
  = TreeMap (Map SafeFilePath TreeEntry)
  -- In the future, consider allowing more lax parsing
  -- See: https://www.fpcomplete.com/blog/2018/07/pantry-part-2-trees-keys
  -- TreeTarball !PackageTarball
  deriving (Show, Eq, Ord)

renderTree :: Tree -> ByteString
renderTree = BL.toStrict . toLazyByteString . go
 where
  go :: Tree -> Builder
  go (TreeMap m) = "map:" <> Map.foldMapWithKey goEntry m

  goEntry sfp (TreeEntry (BlobKey sha (FileSize size')) ft) =
    netstring (unSafeFilePath sfp) <>
    byteString (SHA256.toRaw sha) <>
    netword size' <>
    (case ft of
      FTNormal -> "N"
      FTExecutable -> "X")

netstring :: Text -> Builder
netstring t =
  let bs = encodeUtf8 t
  in  netword (fromIntegral (B.length bs)) <> byteString bs

netword :: Word -> Builder
netword w = wordDec w <> ":"

parseTreeM :: MonadThrow m => (BlobKey, ByteString) -> m (TreeKey, Tree)
parseTreeM (blobKey, blob) =
  case parseTree blob of
    Nothing -> throwM (InvalidTreeFromCasa blobKey blob)
    Just tree -> pure (TreeKey blobKey, tree)

parseTree :: ByteString -> Maybe Tree
parseTree bs1 = do
  tree <- parseTree' bs1
  let bs2 = renderTree tree
  guard $ bs1 == bs2
  Just tree

parseTree' :: ByteString -> Maybe Tree
parseTree' bs0 = do
  entriesBS <- B.stripPrefix "map:" bs0
  TreeMap <$> loop Map.empty entriesBS
 where
  loop !m bs1
    | B.null bs1 = pure m
    | otherwise = do
        (sfpBS, bs2) <- takeNetstring bs1
        sfp <-
          case decodeUtf8' sfpBS of
            Left _ -> Nothing
            Right sfpT -> mkSafeFilePath sfpT
        (sha, bs3) <- takeSha bs2
        (size', bs4) <- takeNetword bs3
        (typeW, bs5) <- B.uncons bs4
        ft <-
          case typeW of
            78 -> Just FTNormal -- 'N'
            88 -> Just FTExecutable -- 'X'
            _ -> Nothing
        let entry = TreeEntry (BlobKey sha (FileSize (fromIntegral size'))) ft
        loop (Map.insert sfp entry m) bs5

  takeNetstring bs1 = do
    (size', bs2) <- takeNetword bs1
    guard $ B.length bs2 >= size'
    Just $ B.splitAt size' bs2

  takeSha bs = do
    let (x, y) = B.splitAt 32 bs
    x' <- either (const Nothing) Just (SHA256.fromRaw x)
    Just (x', y)

  takeNetword =
    go 0
   where
    go !accum bs = do
      (next, rest) <- B.uncons bs
      if
        | next == 58 -> pure (accum, rest) -- ':'
        | next >= 48 && next <= 57 ->
            go
              (accum * 10 + fromIntegral (next - 48))
              rest
        | otherwise -> Nothing

    {-
data PackageTarball = PackageTarball
  { ptBlob :: !BlobKey
  -- ^ Contains the tarball itself
  , ptCabal :: !BlobKey
  -- ^ Contains the cabal file contents
  , ptSubdir :: !FilePath
  -- ^ Subdir containing the files we want for this package.
  --
  -- There must be precisely one file with a @.cabal@ file extension
  -- located there. Thanks to Hackage revisions, its contents will be
  -- overwritten by the value of @ptCabal@.
  }
  deriving Show
    -}

-- | This is almost a copy of Cabal's parser for package identifiers,
-- the main difference is in the fact that Stack requires version to be
-- present while Cabal uses "null version" as a default value
--
-- @since 0.1.0.0
parsePackageIdentifier :: String -> Maybe PackageIdentifier
parsePackageIdentifier = either (const Nothing) Just . explicitEitherParsec (packageIdentifierParsec <* Parse.eof)

packageIdentifierParsec :: ParsecParser PackageIdentifier
packageIdentifierParsec = do
  ident@(PackageIdentifier _ v) <- parsec

  -- version is a required component of a package identifier for Stack
  guard (v /= nullVersion)

  pure ident

-- | Parse a package name from a 'String'.
--
-- @since 0.1.0.0
parsePackageName :: String -> Maybe PackageName
parsePackageName = Distribution.Text.simpleParse

-- | Parse a package name from a 'String' throwing on failure
--
-- @since 0.1.0.0
parsePackageNameThrowing :: MonadThrow m => String -> m PackageName
parsePackageNameThrowing str =
  case parsePackageName str of
    Nothing -> throwM $ PackageNameParseFail $ T.pack str
    Just pn -> pure pn

-- | Parse a version from a 'String'.
--
-- @since 0.1.0.0
parseVersion :: String -> Maybe Version
parseVersion = Distribution.Text.simpleParse

-- | Parse a package version from a 'String' throwing on failure
--
-- @since 0.1.0.0
parseVersionThrowing :: MonadThrow m => String -> m Version
parseVersionThrowing str =
  case parseVersion str of
    Nothing -> throwM $ PackageVersionParseFail $ T.pack str
    Just v -> pure v

-- | Parse a version range from a 'String'.
--
-- @since 0.1.0.0
parseVersionRange :: String -> Maybe VersionRange
parseVersionRange = Distribution.Text.simpleParse

-- | Parse a module name from a 'String'.
--
-- @since 0.1.0.0
parseModuleName :: String -> Maybe ModuleName
parseModuleName = Distribution.Text.simpleParse

-- | Parse a flag name from a 'String'.
--
-- @since 0.1.0.0
parseFlagName :: String -> Maybe FlagName
parseFlagName = Distribution.Text.simpleParse

-- | Render a package name as a 'String'.
--
-- @since 0.1.0.0
packageNameString :: PackageName -> String
packageNameString = unPackageName

-- | Render a package identifier as a 'String'.
--
-- @since 0.1.0.0
packageIdentifierString :: PackageIdentifier -> String
packageIdentifierString = Distribution.Text.display

-- | Render a version as a 'String'.
--
-- @since 0.1.0.0
versionString :: Version -> String
versionString = Distribution.Text.display

-- | Render a flag name as a 'String'.
--
-- @since 0.1.0.0
flagNameString :: FlagName -> String
flagNameString = unFlagName

-- | Render a module name as a 'String'.
--
-- @since 0.1.0.0
moduleNameString :: ModuleName -> String
moduleNameString = Distribution.Text.display

data OptionalSubdirs
  = OSSubdirs !(NonEmpty Text)
  | OSPackageMetadata !Text !RawPackageMetadata
  -- ^ subdirectory and package metadata
  deriving (Show, Eq, Generic)

instance NFData OptionalSubdirs

-- | Metadata provided by a config file for archives and repos. This information
-- can be used for optimized lookups of information like package identifiers, or
-- for validating that the user configuration has the expected information.
--
-- @since 0.1.0.0
data RawPackageMetadata = RawPackageMetadata
  { rpmName :: !(Maybe PackageName)
    -- ^ Package name in the cabal file
    --
    -- @since 0.1.0.0
  , rpmVersion :: !(Maybe Version)
    -- ^ Package version in the cabal file
    --
    -- @since 0.1.0.0
  , rpmTreeKey :: !(Maybe TreeKey)
    -- ^ Tree key of the loaded up package
    --
    -- @since 0.1.0.0
  }
  deriving (Show, Eq, Ord, Generic, Typeable)

instance NFData RawPackageMetadata

instance Display RawPackageMetadata where
  display rpm = fold $ intersperse ", " $ catMaybes
    [ (\name -> "name == " <> fromString (packageNameString name)) <$> rpmName rpm
    , (\version -> "version == " <> fromString (versionString version)) <$> rpmVersion rpm
    , (\tree -> "tree == " <> display tree) <$> rpmTreeKey rpm
    ]

-- | Exact metadata specifying concrete package
--
-- @since 0.1.0.0
data PackageMetadata = PackageMetadata
  { pmIdent :: !PackageIdentifier
    -- ^ Package identifier in the cabal file
    --
    -- @since 0.1.0.0
  , pmTreeKey :: !TreeKey
    -- ^ Tree key of the loaded up package
    --
    -- @since 0.1.0.0
  }
  deriving (Show, Eq, Ord, Generic, Typeable)

-- i PackageMetadata
instance NFData PackageMetadata

instance Display PackageMetadata where
  display pm = fold $ intersperse ", "
    [ "ident == " <> fromString (packageIdentifierString $ pmIdent pm)
    , "tree == " <> display (pmTreeKey pm)
    ]

parsePackageMetadata :: Object -> WarningParser PackageMetadata
parsePackageMetadata o = do
  _oldCabalFile :: Maybe BlobKey <- o ..:? "cabal-file"
  pantryTree :: BlobKey <- o ..: "pantry-tree"
  CabalString pkgName <- o ..: "name"
  CabalString pkgVersion <- o ..: "version"
  let pmTreeKey = TreeKey pantryTree
      pmIdent = PackageIdentifier {..}
  pure PackageMetadata {..}


-- | Convert package metadata to its "raw" equivalent.
--
-- @since 0.1.0.0
toRawPM :: PackageMetadata -> RawPackageMetadata
toRawPM pm = RawPackageMetadata (Just name) (Just version) (Just $ pmTreeKey pm)
 where
  PackageIdentifier name version = pmIdent pm

-- | File path relative to the configuration file it was parsed from
--
-- @since 0.1.0.0
newtype RelFilePath = RelFilePath Text
  deriving (Show, ToJSON, FromJSON, Eq, Ord, Generic, Typeable, NFData, Display)

-- | Location that an archive is stored at
--
-- @since 0.1.0.0
data ArchiveLocation
  = ALUrl !Text
    -- ^ Archive stored at an HTTP(S) URL
    --
    -- @since 0.1.0.0
  | ALFilePath !(ResolvedPath File)
    -- ^ Archive stored at a local file path
    --
    -- @since 0.1.0.0
  deriving (Show, Eq, Ord, Generic, Typeable)

instance NFData ArchiveLocation

instance Display ArchiveLocation where
  display (ALUrl url) = display url
  display (ALFilePath resolved) =
    fromString $ toFilePath $ resolvedAbsolute resolved

instance Pretty ArchiveLocation where
  pretty (ALUrl url) = style Url (fromString $ T.unpack url)
  pretty (ALFilePath resolved) = pretty $ resolvedAbsolute resolved

parseArchiveLocationObject ::
     Object
  -> WarningParser (Unresolved ArchiveLocation)
parseArchiveLocationObject o =
      ((o ..: "url") >>= either (fail . T.unpack) pure . validateUrl)
  <|> ((o ..: "filepath") >>= either (fail . T.unpack) pure . validateFilePath)
  <|> ((o ..: "archive") >>= either (fail . T.unpack) pure . parseArchiveLocationText)
  <|> ((o ..: "location") >>= either (fail . T.unpack) pure . parseArchiveLocationText)

parseArchiveLocationText :: Text -> Either Text (Unresolved ArchiveLocation)
parseArchiveLocationText t =
  case validateUrl t of
    Left e1 ->
      case validateFilePath t of
        Left e2 -> Left $ T.unlines
          [ "Invalid archive location, neither a URL nor a file path"
          , "  URL error: " <> e1
          , "  File path error: " <> e2
          ]
        Right x -> Right x
    Right x -> Right x

validateUrl :: Text -> Either Text (Unresolved ArchiveLocation)
validateUrl t =
  case parseRequest $ T.unpack t of
    Left _ -> Left $ "Could not parse URL: " <> t
    Right _ -> pure $ pure $ ALUrl t

validateFilePath :: Text -> Either Text (Unresolved ArchiveLocation)
validateFilePath t =
  if any (`T.isSuffixOf` t) (T.words ".zip .tar .tar.gz")
    then pure $ Unresolved $ \case
      Nothing -> throwIO $ InvalidFilePathSnapshot t
      Just dir -> do
        abs' <- resolveFile dir $ T.unpack t
        pure $ ALFilePath $ ResolvedPath (RelFilePath t) abs'
    else Left $ "Does not have an archive file extension: " <> t

instance ToJSON RawPackageLocation where
  toJSON (RPLImmutable rpli) = toJSON rpli
  toJSON (RPLMutable resolved) = toJSON (resolvedRelative resolved)

instance FromJSON (WithJSONWarnings (Unresolved (NonEmpty RawPackageLocation))) where
  parseJSON v =
    (fmap . fmap . fmap . fmap) RPLImmutable (parseJSON v) <|>
    (noJSONWarnings . mkMutable <$> parseJSON v)
   where
    mkMutable :: Text -> Unresolved (NonEmpty RawPackageLocation)
    mkMutable t = Unresolved $ \mdir -> do
      case mdir of
        Nothing -> throwIO $ MutablePackageLocationFromUrl t
        Just dir -> do
          abs' <- resolveDir dir $ T.unpack t
          pure $ pure $ RPLMutable $ ResolvedPath (RelFilePath t) abs'

instance ToJSON RawPackageLocationImmutable where
  toJSON (RPLIHackage pir mtree) = object $
    ("hackage" .= pir) : maybe [] (\tree -> ["pantry-tree" .= tree]) mtree
  toJSON (RPLIArchive (RawArchive loc msha msize subdir) rpm) = object $ concat
    [ case loc of
        ALUrl url -> ["url" .= url]
        ALFilePath resolved -> ["filepath" .= resolvedRelative resolved]
    , maybe [] (\sha -> ["sha256" .= sha]) msha
    , maybe [] (\size' -> ["size" .= size']) msize
    , [ "subdir" .= subdir | not (T.null subdir) ]
    , rpmToPairs rpm
    ]
  toJSON (RPLIRepo (Repo url commit typ subdir) rpm) = object $ concat
    [ [ urlKey .= url
      , "commit" .= commit
      ]
    , ["subdir" .= subdir | not (T.null subdir) ]
    , rpmToPairs rpm
    ]
   where
    urlKey =
      case typ of
        RepoGit -> "git"
        RepoHg  -> "hg"

rpmToPairs :: RawPackageMetadata -> [(AesonKey, Value)]
rpmToPairs (RawPackageMetadata mname mversion mtree) = concat
  [ maybe [] (\name -> ["name" .= CabalString name]) mname
  , maybe [] (\version -> ["version" .= CabalString version]) mversion
  , maybe [] (\tree -> ["pantry-tree" .= tree]) mtree
  ]

instance FromJSON (WithJSONWarnings (Unresolved PackageLocationImmutable)) where
  parseJSON v =
        repoObject v
    <|> archiveObject v
    <|> hackageObject v
    <|> github v
    <|> fail ("Could not parse a UnresolvedPackageLocationImmutable from: " ++ show v)
   where
    repoObject ::
         Value
      -> Parser (WithJSONWarnings (Unresolved PackageLocationImmutable))
    repoObject =
      withObjectWarnings "UnresolvedPackageLocationImmutable.PLIRepo" $ \o -> do
        pm <- parsePackageMetadata o
        repoSubdir <- o ..:? "subdir" ..!= ""
        repoCommit <- o ..: "commit"
        (repoType, repoUrl) <-
            (o ..: "git" >>= \url -> pure (RepoGit, url)) <|>
            (o ..: "hg" >>= \url -> pure (RepoHg, url))
        pure $ pure $ PLIRepo Repo {..} pm

    archiveObject =
      withObjectWarnings "UnresolvedPackageLocationImmutable.PLIArchive" $ \o -> do
        pm <- parsePackageMetadata o
        Unresolved mkArchiveLocation <- parseArchiveLocationObject o
        archiveHash <- o ..: "sha256"
        archiveSize <- o ..: "size"
        archiveSubdir <- o ..:? "subdir" ..!= ""
        pure $ Unresolved $ \mdir -> do
          archiveLocation <- mkArchiveLocation mdir
          pure $ PLIArchive Archive {..} pm

    hackageObject =
      withObjectWarnings "UnresolvedPackagelocationimmutable.PLIHackage (Object)" $ \o -> do
        treeKey <- o ..: "pantry-tree"
        htxt <- o ..: "hackage"
        case parseHackageText htxt of
          Left e -> fail $ show e
          Right (pkgIdentifier, blobKey) ->
            pure $ pure $ PLIHackage pkgIdentifier blobKey (TreeKey treeKey)

    github =
      withObjectWarnings "UnresolvedPackagelocationimmutable.PLIArchive:github" (\o -> do
        pm <- parsePackageMetadata o
        GitHubRepo ghRepo <- o ..: "github"
        commit <- o ..: "commit"
        let archiveLocation = ALUrl $ T.concat
              [ "https://github.com/"
              , ghRepo
              , "/archive/"
              , commit
              , ".tar.gz"
              ]
        archiveHash <- o ..: "sha256"
        archiveSize <- o ..: "size"
        archiveSubdir <- o ..:? "subdir" ..!= ""
        pure $ pure $ PLIArchive Archive {..} pm)

instance FromJSON (WithJSONWarnings (Unresolved (NonEmpty RawPackageLocationImmutable))) where
  parseJSON v =
        http v
    <|> hackageText v
    <|> hackageObject v
    <|> repo v
    <|> archiveObject v
    <|> github v
    <|> fail ("Could not parse a UnresolvedRawPackageLocationImmutable from: " ++ show v)
   where
    http :: Value -> Parser (WithJSONWarnings (Unresolved (NonEmpty RawPackageLocationImmutable)))
    http = withText "UnresolvedPackageLocationImmutable.RPLIArchive (Text)" $ \t ->
      case parseArchiveLocationText t of
        Left _ -> fail $ "Invalid archive location: " ++ T.unpack t
        Right (Unresolved mkArchiveLocation) ->
          pure $ noJSONWarnings $ Unresolved $ \mdir -> do
            raLocation <- mkArchiveLocation mdir
            let raHash = Nothing
                raSize = Nothing
                raSubdir = T.empty
            pure $ pure $ RPLIArchive RawArchive {..} rpmEmpty

    hackageText = withText "UnresolvedPackageLocationImmutable.UPLIHackage (Text)" $ \t ->
      case parsePackageIdentifierRevision t of
        Left e -> fail $ show e
        Right pir -> pure $ noJSONWarnings $ pure $ pure $ RPLIHackage pir Nothing

    hackageObject = withObjectWarnings "UnresolvedPackageLocationImmutable.UPLIHackage" $ \o -> pure . pure <$> (RPLIHackage
      <$> o ..: "hackage"
      <*> o ..:? "pantry-tree")

    optionalSubdirs :: Object -> WarningParser OptionalSubdirs
    optionalSubdirs o =
      -- if subdirs exists, it needs to be valid
      case HM.lookup "subdirs" o of
        Just v' -> do
          tellJSONField "subdirs"
          subdirs <- lift $ parseJSON v'
          case NE.nonEmpty subdirs of
            Nothing -> fail "Invalid empty subdirs"
            Just x -> pure $ OSSubdirs x
        Nothing -> OSPackageMetadata
          <$> o ..:? "subdir" ..!= T.empty
          <*> (   rawPackageMetadataHelper
              <$> (fmap unCabalString <$> (o ..:? "name"))
              <*> (fmap unCabalString <$> (o ..:? "version"))
              <*> o ..:? "pantry-tree"
              <*> o ..:? "cabal-file"
              )

    rawPackageMetadataHelper ::
         Maybe PackageName
      -> Maybe Version
      -> Maybe TreeKey
      -> Maybe BlobKey
      -> RawPackageMetadata
    rawPackageMetadataHelper name version pantryTree _ignoredCabalFile =
      RawPackageMetadata name version pantryTree

    repo = withObjectWarnings "UnresolvedPackageLocationImmutable.UPLIRepo" $ \o -> do
      (repoType, repoUrl) <-
        ((RepoGit, ) <$> o ..: "git") <|>
        ((RepoHg, ) <$> o ..: "hg")
      repoCommit <- o ..: "commit"
      os <- optionalSubdirs o
      pure $ pure $ NE.map (\(repoSubdir, pm) -> RPLIRepo Repo {..} pm) (osToRpms os)

    archiveObject = withObjectWarnings "UnresolvedPackageLocationImmutable.RPLIArchive" $ \o -> do
      Unresolved mkArchiveLocation <- parseArchiveLocationObject o
      raHash <- o ..:? "sha256"
      raSize <- o ..:? "size"
      os <- optionalSubdirs o
      pure $ Unresolved $ \mdir -> do
        raLocation <- mkArchiveLocation mdir
        pure $ NE.map (\(raSubdir, pm) -> RPLIArchive RawArchive {..} pm) (osToRpms os)

    github = withObjectWarnings "PLArchive:github" $ \o -> do
      GitHubRepo ghRepo <- o ..: "github"
      commit <- o ..: "commit"
      let raLocation = ALUrl $ T.concat
            [ "https://github.com/"
            , ghRepo
            , "/archive/"
            , commit
            , ".tar.gz"
            ]
      raHash <- o ..:? "sha256"
      raSize <- o ..:? "size"
      os <- optionalSubdirs o
      pure $ pure $
        NE.map (\(raSubdir, pm) -> RPLIArchive RawArchive {..} pm) (osToRpms os)

-- | Returns pairs of subdirectory and 'PackageMetadata'.
osToRpms :: OptionalSubdirs -> NonEmpty (Text, RawPackageMetadata)
osToRpms (OSSubdirs subdirs) = NE.map (, rpmEmpty) subdirs
osToRpms (OSPackageMetadata subdir rpm) = pure (subdir, rpm)

rpmEmpty :: RawPackageMetadata
rpmEmpty = RawPackageMetadata Nothing Nothing Nothing

-- | Newtype wrapper for easier JSON integration with Cabal types.
--
-- @since 0.1.0.0
newtype CabalString a = CabalString { unCabalString :: a }
  deriving (Show, Eq, Ord, Typeable)

-- I'd like to use coerce here, but can't due to roles. unsafeCoerce
-- could work, but let's avoid unsafe code.

-- | Wrap the keys in a 'Map' with a 'CabalString' to get a 'ToJSON'
-- instance.
--
-- @since 0.1.0.0
toCabalStringMap :: Map a v -> Map (CabalString a) v
toCabalStringMap = Map.mapKeysMonotonic CabalString

-- | Unwrap the 'CabalString' from the keys in a 'Map' to use a
-- 'FromJSON' instance.
--
-- @since 0.1.0.0
unCabalStringMap :: Map (CabalString a) v -> Map a v
unCabalStringMap = Map.mapKeysMonotonic unCabalString

instance Distribution.Pretty.Pretty a => ToJSON (CabalString a) where
  toJSON = toJSON . Distribution.Text.display . unCabalString

instance Distribution.Pretty.Pretty a => ToJSONKey (CabalString a) where
  toJSONKey = toJSONKeyText $ T.pack . Distribution.Text.display . unCabalString

instance forall a. IsCabalString a => FromJSON (CabalString a) where
  parseJSON = withText name $ \t ->
    case cabalStringParser $ T.unpack t of
      Nothing -> fail $ "Invalid " ++ name ++ ": " ++ T.unpack t
      Just x -> pure $ CabalString x
   where
    name = cabalStringName (Nothing :: Maybe a)

instance forall a. IsCabalString a => FromJSONKey (CabalString a) where
  fromJSONKey =
    FromJSONKeyTextParser $ \t ->
    case cabalStringParser $ T.unpack t of
      Nothing -> fail $ "Invalid " ++ name ++ ": " ++ T.unpack t
      Just x -> pure $ CabalString x
   where
    name = cabalStringName (Nothing :: Maybe a)

class IsCabalString a where
  cabalStringName :: proxy a -> String
  cabalStringParser :: String -> Maybe a

instance IsCabalString PackageName where
  cabalStringName _ = "package name"
  cabalStringParser = parsePackageName

instance IsCabalString Version where
  cabalStringName _ = "version"
  cabalStringParser = parseVersion

instance IsCabalString VersionRange where
  cabalStringName _ = "version range"
  cabalStringParser = parseVersionRange

instance IsCabalString PackageIdentifier where
  cabalStringName _ = "package identifier"
  cabalStringParser = parsePackageIdentifier

instance IsCabalString FlagName where
  cabalStringName _ = "flag name"
  cabalStringParser = parseFlagName

-- | What to use for running hpack
--
-- @since 0.1.0.0
data HpackExecutable
  = HpackBundled
    -- ^ Compiled in library
  | HpackCommand !FilePath
    -- ^ Executable at the provided path
  deriving (Show, Read, Eq, Ord)


-- | Which compiler a snapshot wants to use. The build tool may elect to do some
-- fuzzy matching of versions (e.g., allowing different patch versions).
--
-- @since 0.1.0.0
data WantedCompiler
  = WCGhc !Version
  | WCGhcGit !Text !Text
  | WCGhcjs
      !Version
      !Version
    -- ^ GHCJS version followed by GHC version
  deriving (Show, Eq, Ord, Generic)

instance NFData WantedCompiler

instance Display WantedCompiler where
  display (WCGhc vghc) = "ghc-" <> fromString (versionString vghc)
  display (WCGhcjs vghcjs vghc) =
       "ghcjs-"
    <> fromString (versionString vghcjs)
    <> "_ghc-" <> fromString (versionString vghc)
  display (WCGhcGit commit flavour) =
    "ghc-git-" <> display commit <> "-" <> display flavour

instance ToJSON WantedCompiler where
  toJSON = toJSON . utf8BuilderToText . display

instance FromJSON WantedCompiler where
  parseJSON =
    withText "WantedCompiler" $ either (fail . show) pure . parseWantedCompiler

instance FromJSONKey WantedCompiler where
  fromJSONKey =
    FromJSONKeyTextParser $ \t ->
    case parseWantedCompiler t of
      Left e -> fail $ "Invalid WantedCompiler " ++ show t ++ ": " ++ show e
      Right x -> pure x

-- | Parse a 'Text' into a 'WantedCompiler' value.
--
-- @since 0.1.0.0
parseWantedCompiler :: Text -> Either PantryException WantedCompiler
parseWantedCompiler t0 = maybe (Left $ InvalidWantedCompiler t0) Right $
  case T.stripPrefix "ghcjs-" t0 of
    Just t1 -> parseGhcjs t1
    Nothing -> case T.stripPrefix "ghc-git-" t0 of
       Just t1 -> parseGhcGit t1
       Nothing -> T.stripPrefix "ghc-" t0 >>= parseGhc
 where
  parseGhcjs t1 = do
    let (ghcjsVT, t2) = T.break (== '_') t1
    ghcjsV <- parseVersion $ T.unpack ghcjsVT
    ghcVT <- T.stripPrefix "_ghc-" t2
    ghcV <- parseVersion $ T.unpack ghcVT
    pure $ WCGhcjs ghcjsV ghcV
  parseGhcGit t1 = do
    let (commit, flavour) = T.break (== '-') t1
    pure $ WCGhcGit commit (T.drop 1 flavour)
  parseGhc = fmap WCGhc . parseVersion . T.unpack

instance FromJSON (WithJSONWarnings (Unresolved RawSnapshotLocation)) where
  parseJSON v = text v <|> obj v
   where
    text :: Value -> Parser (WithJSONWarnings (Unresolved RawSnapshotLocation))
    text =
      withText "UnresolvedSnapshotLocation (Text)" $
        pure . noJSONWarnings . parseRawSnapshotLocation

    obj :: Value -> Parser (WithJSONWarnings (Unresolved RawSnapshotLocation))
    obj = withObjectWarnings "UnresolvedSnapshotLocation (Object)" $ \o ->
      (pure . RSLCompiler <$> o ..: "compiler") <|>
      ((\x y -> pure $ RSLUrl x y) <$> o ..: "url" <*> blobKey o) <|>
      (parseRawSnapshotLocationPath <$> o ..: "filepath")

    blobKey o = do
      msha <- o ..:? "sha256"
      msize <- o ..:? "size"
      case (msha, msize) of
        (Nothing, Nothing) -> pure Nothing
        (Just sha, Just size') -> pure $ Just $ BlobKey sha size'
        (Just _sha, Nothing) -> fail "You must also specify the file size"
        (Nothing, Just _) -> fail "You must also specify the file's SHA256"

instance Display SnapshotLocation where
  display (SLCompiler compiler) = display compiler
  display (SLUrl url blob) = display url <> " (" <> display blob <> ")"
  display (SLFilePath resolved) = display (resolvedRelative resolved)

-- | Parse a 'Text' into an 'Unresolved' 'RawSnapshotLocation'.
--
-- @since 0.1.0.0
parseRawSnapshotLocation :: Text -> Unresolved RawSnapshotLocation
parseRawSnapshotLocation t0 = fromMaybe (parseRawSnapshotLocationPath t0) $
  either (const Nothing) (Just . pure . RSLCompiler) (parseWantedCompiler t0) <|>
  (pure . RSLSynonym <$> parseSnapName t0) <|>
  parseGitHub <|>
  parseUrl
 where
  parseGitHub = do
    t1 <- T.stripPrefix "github:" t0
    let (user, t2) = T.break (== '/') t1
    t3 <- T.stripPrefix "/" t2
    let (repo, t4) = T.break (== ':') t3
    path <- T.stripPrefix ":" t4
    Just $ pure $ githubSnapshotLocation user repo path

  parseUrl = parseRequest (T.unpack t0) $> pure (RSLUrl t0 Nothing)

parseRawSnapshotLocationPath :: Text -> Unresolved RawSnapshotLocation
parseRawSnapshotLocationPath t =
  Unresolved $ \case
    Nothing -> throwIO $ InvalidFilePathSnapshot t
    Just dir -> do
      abs' <- resolveFile dir (T.unpack t) `catchAny` \_ -> throwIO (InvalidSnapshotLocation dir t)
      pure $ RSLFilePath $ ResolvedPath (RelFilePath t) abs'

githubSnapshotLocation :: Text -> Text -> Text -> RawSnapshotLocation
githubSnapshotLocation user repo path =
  let url = T.concat
        [ "https://raw.githubusercontent.com/"
        , user
        , "/"
        , repo
        , "/master/"
        , path
        ]
  in  RSLUrl url Nothing

defUser :: Text
defUser = "commercialhaskell"

defRepo :: Text
defRepo = "stackage-snapshots"

-- | Default location of snapshot synonyms, i.e. commercialhaskell's GitHub
-- repository.
--
-- @since 0.5.0.0
defaultSnapshotLocation ::
     SnapName
  -> RawSnapshotLocation
defaultSnapshotLocation (LTS x y) =
  githubSnapshotLocation defUser defRepo $
  utf8BuilderToText $
  "lts/" <> display x <> "/" <> display y <> ".yaml"
defaultSnapshotLocation (Nightly date) =
     githubSnapshotLocation defUser defRepo
  $  utf8BuilderToText
  $  "nightly/"
  <> display year
  <> "/"
  <> display month
  <> "/"
  <> display day
  <> ".yaml"
 where
  (year, month, day) = toGregorian date

-- | A snapshot synonym. It is expanded according to the field
-- 'snapshotLocation' of a 'PantryConfig'.
--
-- @ since 0.5.0.0
data SnapName
    = LTS
        !Int -- ^ Major version
        !Int -- ^ Minor version
      -- ^ LTS Haskell snapshot, displayed as @"lts-maj.min"@.
      --
      -- @since 0.5.0.0
    | Nightly !Day
      -- ^ Stackage Nightly snapshot, displayed as @"nighly-YYYY-MM-DD"@.
      --
      -- @since 0.5.0.0
    deriving (Eq, Ord, Generic)

instance NFData SnapName

instance Display SnapName where
  display (LTS x y) = "lts-" <> display x <> "." <> display y
  display (Nightly date) = "nightly-" <> displayShow date

instance Show SnapName where
  show = T.unpack . utf8BuilderToText . display

instance ToJSON SnapName where
  toJSON syn = String $ utf8BuilderToText $ display syn

-- | Parse the short representation of a 'SnapName'.
--
-- @since 0.5.0.0
parseSnapName :: MonadThrow m => Text -> m SnapName
parseSnapName t0 =
  case lts <|> nightly of
    Nothing -> throwM $ ParseSnapNameException t0
    Just sn -> pure sn
 where
  lts = do
    t1 <- T.stripPrefix "lts-" t0
    Right (x, t2) <- Just $ decimal t1
    t3 <- T.stripPrefix "." t2
    Right (y, "") <- Just $ decimal t3
    pure $ LTS x y
  nightly = do
    t1 <- T.stripPrefix "nightly-" t0
    Nightly <$> readMaybe (T.unpack t1)

-- | Where to load a snapshot from in raw form (RSUrl could have a missing
-- BlobKey)
--
-- @since 0.1.0.0
data RawSnapshotLocation
  = RSLCompiler !WantedCompiler
    -- ^ Don't use an actual snapshot, just a version of the compiler with its
    -- shipped packages.
    --
    -- @since 0.1.0.0
  | RSLUrl !Text !(Maybe BlobKey)
    -- ^ Download the snapshot from the given URL. The optional 'BlobKey' is
    -- used for reproducibility.
    --
    -- @since 0.1.0.0
  | RSLFilePath !(ResolvedPath File)
    -- ^ Snapshot at a local file path.
    --
    -- @since 0.1.0.0
  | RSLSynonym !SnapName
    -- ^ Snapshot synonym (LTS/Nightly).
    --
    -- @since 0.5.0.0
  deriving (Show, Eq, Ord, Generic)

instance NFData RawSnapshotLocation

instance Display RawSnapshotLocation where
  display (RSLCompiler compiler) = display compiler
  display (RSLUrl url Nothing) = display url
  display (RSLUrl url (Just blob)) = display url <> " (" <> display blob <> ")"
  display (RSLFilePath resolved) = display (resolvedRelative resolved)
  display (RSLSynonym syn) = display syn

instance Pretty RawSnapshotLocation where
  pretty (RSLCompiler compiler) = fromString . T.unpack $ textDisplay compiler
  pretty (RSLUrl url Nothing) = style Url (fromString $ T.unpack url)
  pretty (RSLUrl url (Just blob)) = fillSep
    [ style Url (fromString $ T.unpack  url)
    , parens (fromString . T.unpack $ textDisplay blob)
    ]
  pretty (RSLFilePath resolved) =
    style File (fromString . T.unpack $ textDisplay (resolvedRelative resolved))
  pretty (RSLSynonym syn) =
    style Shell (fromString . T.unpack $ textDisplay syn)

instance ToJSON RawSnapshotLocation where
  toJSON (RSLCompiler compiler) = object ["compiler" .= compiler]
  toJSON (RSLUrl url mblob) = object
    $ "url" .= url
    : maybe [] blobKeyPairs mblob
  toJSON (RSLFilePath resolved) =
    object ["filepath" .= resolvedRelative resolved]
  toJSON (RSLSynonym syn) = toJSON syn

-- | Where to load a snapshot from.
--
-- @since 0.1.0.0
data SnapshotLocation
  = SLCompiler !WantedCompiler
    -- ^ Don't use an actual snapshot, just a version of the compiler
    -- with its shipped packages.
    --
    -- @since 0.1.0.0
  | SLUrl !Text !BlobKey
    -- ^ Download the snapshot from the given URL. The optional
    -- 'BlobKey' is used for reproducibility.
    --
    -- @since 0.1.0.0
  | SLFilePath !(ResolvedPath File)
    -- ^ Snapshot at a local file path.
    --
    -- @since 0.1.0.0
  deriving (Show, Eq, Ord, Generic)

instance NFData SnapshotLocation

instance ToJSON SnapshotLocation where
  toJSON sl = toJSON (toRawSL sl)

instance FromJSON (WithJSONWarnings (Unresolved SnapshotLocation)) where
  parseJSON v = file v <|> url v <|> compiler v
   where
    file = withObjectWarnings "SLFilepath" $ \o -> do
      ufp <- o ..: "filepath"
      pure $ Unresolved $ \case
        Nothing -> throwIO $ InvalidFilePathSnapshot ufp
        Just dir -> do
          absolute <- resolveFile dir (T.unpack ufp)
          let fp = ResolvedPath (RelFilePath ufp) absolute
          pure $ SLFilePath fp
    url = withObjectWarnings "SLUrl" $ \o -> do
      url' <- o ..: "url"
      sha <- o ..: "sha256"
      size <- o ..: "size"
      pure $ Unresolved $ \_ -> pure $ SLUrl url' (BlobKey sha size)
    compiler = withObjectWarnings "SLCompiler" $ \o -> do
      c <- o ..: "compiler"
      pure $ Unresolved $ \_ -> pure $ SLCompiler c

-- | Convert snapshot location to its "raw" equivalent.
--
-- @since 0.1.0.0
toRawSL :: SnapshotLocation -> RawSnapshotLocation
toRawSL (SLCompiler c) = RSLCompiler c
toRawSL (SLUrl url blob) = RSLUrl url (Just blob)
toRawSL (SLFilePath fp) = RSLFilePath fp

-- | A flattened representation of all the layers in a snapshot.
--
-- @since 0.1.0.0
data RawSnapshot = RawSnapshot
  { rsCompiler :: !WantedCompiler
    -- ^ The compiler wanted for this snapshot.
  , rsPackages :: !(Map PackageName RawSnapshotPackage)
    -- ^ Packages available in this snapshot for installation. This will be
    -- applied on top of any globally available packages.
  , rsDrop :: !(Set PackageName)
    -- ^ Global packages that should be dropped/ignored.
  }

-- | A flattened representation of all the layers in a snapshot.
--
-- @since 0.1.0.0
data Snapshot = Snapshot
  { snapshotCompiler :: !WantedCompiler
    -- ^ The compiler wanted for this snapshot.
  , snapshotPackages :: !(Map PackageName SnapshotPackage)
    -- ^ Packages available in this snapshot for installation. This will be
    -- applied on top of any globally available packages.
  , snapshotDrop :: !(Set PackageName)
    -- ^ Global packages that should be dropped/ignored.
  }

-- | Settings for a package found in a snapshot.
--
-- @since 0.1.0.0
data RawSnapshotPackage = RawSnapshotPackage
  { rspLocation :: !RawPackageLocationImmutable
    -- ^ Where to get the package from
  , rspFlags :: !(Map FlagName Bool)
    -- ^ Same as 'slFlags'
  , rspHidden :: !Bool
    -- ^ Same as 'slHidden'
  , rspGhcOptions :: ![Text]
    -- ^ Same as 'slGhcOptions'
  }

-- | Settings for a package found in a snapshot.
--
-- @since 0.1.0.0
data SnapshotPackage = SnapshotPackage
  { spLocation :: !PackageLocationImmutable
    -- ^ Where to get the package from
  , spFlags :: !(Map FlagName Bool)
    -- ^ Same as 'slFlags'
  , spHidden :: !Bool
    -- ^ Same as 'slHidden'
  , spGhcOptions :: ![Text]
    -- ^ Same as 'slGhcOptions'
  }
  deriving Show

-- | A single layer of a snapshot, i.e. a specific YAML configuration file.
--
-- @since 0.1.0.0
data RawSnapshotLayer = RawSnapshotLayer
  { rslParent :: !RawSnapshotLocation
    -- ^ The sl to extend from. This is either a specific compiler, or a
    -- @SnapshotLocation@ which gives us more information (like packages).
    -- Ultimately, we'll end up with a @CompilerVersion@.
    --
    -- @since 0.1.0.0
  , rslCompiler :: !(Maybe WantedCompiler)
    -- ^ Override the compiler specified in 'slParent'. Must be 'Nothing' if
    -- using 'SLCompiler'.
    --
    -- @since 0.1.0.0
  , rslLocations :: ![RawPackageLocationImmutable]
    -- ^ Where to grab all of the packages from.
    --
    -- @since 0.1.0.0
  , rslDropPackages :: !(Set PackageName)
    -- ^ Packages present in the parent which should not be included here.
    --
    -- @since 0.1.0.0
  , rslFlags :: !(Map PackageName (Map FlagName Bool))
    -- ^ Flag values to override from the defaults
    --
    -- @since 0.1.0.0
  , rslHidden :: !(Map PackageName Bool)
    -- ^ Packages which should be hidden when registering. This will affect, for
    -- example, the import parser in the script command. We use a 'Map' instead
    -- of just a 'Set' to allow overriding the hidden settings in a parent sl.
    --
    -- @since 0.1.0.0
  , rslGhcOptions :: !(Map PackageName [Text])
    -- ^ GHC options per package
    --
    -- @since 0.1.0.0
  , rslPublishTime :: !(Maybe UTCTime)
    -- ^ See 'slPublishTime'
    --
    -- @since 0.1.0.0
  }
  deriving (Show, Eq, Generic)

instance NFData RawSnapshotLayer

instance ToJSON RawSnapshotLayer where
  toJSON rsnap = object $ concat
    [ ["resolver" .= rslParent rsnap]
    , maybe [] (\compiler -> ["compiler" .= compiler]) (rslCompiler rsnap)
    , ["packages" .= rslLocations rsnap]
    , [ "drop-packages" .= Set.map CabalString (rslDropPackages rsnap)
      | not (Set.null (rslDropPackages rsnap))
      ]
    , [ "flags" .= fmap toCabalStringMap (toCabalStringMap (rslFlags rsnap))
      | not(Map.null (rslFlags rsnap))
      ]
    , [ "hidden" .= toCabalStringMap (rslHidden rsnap)
      | not (Map.null (rslHidden rsnap))
      ]
    , [ "ghc-options" .= toCabalStringMap (rslGhcOptions rsnap)
      | not (Map.null (rslGhcOptions rsnap))
      ]
    , maybe [] (\time -> ["publish-time" .= time]) (rslPublishTime rsnap)
    ]

instance FromJSON (WithJSONWarnings (Unresolved RawSnapshotLayer)) where
  parseJSON = withObjectWarnings "Snapshot" $ \o -> do
    _ :: Maybe Text <- o ..:? "name" -- avoid warnings for old snapshot format
    mcompiler <- o ..:? "compiler"
    mresolver <- jsonSubWarningsT $ o ...:? ["snapshot", "resolver"]
    unresolvedSnapshotParent <-
      case (mcompiler, mresolver) of
        (Nothing, Nothing) -> fail "Snapshot must have either resolver or compiler"
        (Just compiler, Nothing) -> pure $ pure (RSLCompiler compiler, Nothing)
        (_, Just (Unresolved usl)) -> pure $ Unresolved $ \mdir -> do
          sl <- usl mdir
          case (sl, mcompiler) of
            (RSLCompiler c1, Just c2) -> throwIO $ InvalidOverrideCompiler c1 c2
            _ -> pure (sl, mcompiler)

    unresolvedLocs <- jsonSubWarningsT (o ..:? "packages" ..!= [])
    rslDropPackages <- Set.map unCabalString <$> (o ..:? "drop-packages" ..!= Set.empty)
    rslFlags <- unCabalStringMap . fmap unCabalStringMap <$> (o ..:? "flags" ..!= Map.empty)
    rslHidden <- unCabalStringMap <$> (o ..:? "hidden" ..!= Map.empty)
    rslGhcOptions <- unCabalStringMap <$> (o ..:? "ghc-options" ..!= Map.empty)
    rslPublishTime <- o ..:? "publish-time"
    pure $ (\rslLocations (rslParent, rslCompiler) -> RawSnapshotLayer {..})
      <$> (concatMap NE.toList <$> sequenceA unresolvedLocs)
      <*> unresolvedSnapshotParent

-- | A single layer of a snapshot, i.e. a specific YAML configuration file.
--
-- @since 0.1.0.0
data SnapshotLayer = SnapshotLayer
  { slParent :: !SnapshotLocation
    -- ^ The sl to extend from. This is either a specific compiler, or a
    -- @SnapshotLocation@ which gives us more information (like packages).
    -- Ultimately, we'll end up with a @CompilerVersion@.
    --
    -- @since 0.1.0.0
  , slCompiler :: !(Maybe WantedCompiler)
    -- ^ Override the compiler specified in 'slParent'. Must be 'Nothing' if
    -- using 'SLCompiler'.
    --
    -- @since 0.1.0.0
  , slLocations :: ![PackageLocationImmutable]
    -- ^ Where to grab all of the packages from.
    --
    -- @since 0.1.0.0
  , slDropPackages :: !(Set PackageName)
    -- ^ Packages present in the parent which should not be included here.
    --
    -- @since 0.1.0.0
  , slFlags :: !(Map PackageName (Map FlagName Bool))
    -- ^ Flag values to override from the defaults
    --
    -- @since 0.1.0.0
  , slHidden :: !(Map PackageName Bool)
    -- ^ Packages which should be hidden when registering. This will affect, for
    -- example, the import parser in the script command. We use a 'Map' instead
    -- of just a 'Set' to allow overriding the hidden settings in a parent sl.
    --
    -- @since 0.1.0.0
  , slGhcOptions :: !(Map PackageName [Text])
    -- ^ GHC options per package
    --
    -- @since 0.1.0.0
  , slPublishTime :: !(Maybe UTCTime)
    -- ^ Publication timestamp for this snapshot. This field is optional, and is
    -- for informational purposes only.
    --
    -- @since 0.1.0.0
  }
  deriving (Show, Eq, Generic)

instance ToJSON SnapshotLayer where
  toJSON snap = object $ concat
    [ ["resolver" .= slParent snap]
    , maybe [] (\compiler -> ["compiler" .= compiler]) (slCompiler snap)
    , ["packages" .= slLocations snap]
    , [ "drop-packages" .= Set.map CabalString (slDropPackages snap)
      | not (Set.null (slDropPackages snap))
      ]
    , [ "flags" .= fmap toCabalStringMap (toCabalStringMap (slFlags snap))
      | not (Map.null (slFlags snap))
      ]
    , [ "hidden" .= toCabalStringMap (slHidden snap)
      | not (Map.null (slHidden snap))
      ]
    , [ "ghc-options" .= toCabalStringMap (slGhcOptions snap)
      | not (Map.null (slGhcOptions snap))
      ]
    , maybe [] (\time -> ["publish-time" .= time]) (slPublishTime snap)
    ]

-- | Convert snapshot layer into its "raw" equivalent.
--
-- @since 0.1.0.0
toRawSnapshotLayer :: SnapshotLayer -> RawSnapshotLayer
toRawSnapshotLayer sl = RawSnapshotLayer
  { rslParent = toRawSL (slParent sl)
  , rslCompiler = slCompiler sl
  , rslLocations = map toRawPLI (slLocations sl)
  , rslDropPackages = slDropPackages sl
  , rslFlags = slFlags sl
  , rslHidden = slHidden sl
  , rslGhcOptions = slGhcOptions sl
  , rslPublishTime = slPublishTime sl
  }

-- | An arbitrary hash for a snapshot, used for finding module names in a
-- snapshot. Mostly intended for Stack's usage.
--
-- @since 0.1.0.0
newtype SnapshotCacheHash = SnapshotCacheHash { unSnapshotCacheHash :: SHA256}
  deriving (Show)

-- | Get the path to the global hints cache file
getGlobalHintsFile :: HasPantryConfig env => RIO env (Path Abs File)
getGlobalHintsFile = do
  root <- view $ pantryConfigL.to pcRootDir
  globalHintsRelFile <- parseRelFile "global-hints-cache.yaml"
  pure $ root </> globalHintsRelFile

-- | Creates BlobKey for an input ByteString
--
-- @since 0.1.0.0
bsToBlobKey :: ByteString -> BlobKey
bsToBlobKey bs =
  BlobKey (SHA256.hashBytes bs) (FileSize (fromIntegral (B.length bs)))

-- | Warn if the package uses 'PCHpack'.
--
-- @since 0.4.0.0
warnMissingCabalFile :: HasLogFunc env => RawPackageLocationImmutable -> RIO env ()
warnMissingCabalFile loc =
  logWarn $
       "DEPRECATED: The package at "
    <> display loc
    <> " does not include a cabal file.\n"
    <> "Instead, it includes an hpack package.yaml file for generating a cabal file.\n"
    <> "This usage is deprecated; please see "
    <> "https://github.com/commercialhaskell/stack/issues/5210.\n"
    <> "Support for this workflow will be removed in the future.\n"
