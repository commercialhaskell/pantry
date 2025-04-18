{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- | An executable to allow a person to inspect in a terminal the form of
-- Pantry's pretty exceptions.
module Main
 ( main
 ) where

import           Data.Aeson.WarningParser ( JSONWarning (..) )
import qualified Data.Conduit.Tar as Tar
import           Data.Maybe ( fromJust )
import qualified Data.Text as T
import qualified Distribution.Parsec.Error as C
import qualified Distribution.Parsec.Position as C
import qualified Distribution.Parsec.Warning as C
import qualified Distribution.Types.PackageName as C
import qualified Distribution.Types.Version as C
import           Options.Applicative
                   ( Parser, (<**>), auto, execParser, fullDesc, header, help
                   , helper, info, long, metavar, option, progDesc, showDefault
                   , strOption, value
                   )
import           Network.HTTP.Types.Status ( Status, mkStatus )
import           Pantry
                   ( ArchiveLocation (..), BlobKey (..), CabalFileInfo (..)
                   , FileSize (..), FuzzyResults (..), Mismatch (..)
                   , PackageName, PantryException (..), PackageIdentifier (..)
                   , PackageIdentifierRevision (..), PackageMetadata (..)
                   , RawPackageLocationImmutable (..), RawPackageMetadata (..)
                   , RawSnapshotLocation (..), RelFilePath (..), Repo (..)
                   , RepoType (..), ResolvedPath (..), Revision (..), SHA256
                   , SafeFilePath, SimpleRepo (..), SnapName (..)
                   , TreeKey (..), Version, WantedCompiler (..), mkSafeFilePath
                   )
import           Pantry.SHA256 ( hashBytes )
import           Path ( File )
import           PathAbsExamples
                   ( pathAbsDirExample, pathAbsFileExample
                   , pathAbsFileExamples
                   )
import           RIO
import qualified RIO.List as L
import           RIO.NonEmpty ( nonEmpty )
import           RIO.PrettyPrint ( pretty, prettyError )
import           RIO.PrettyPrint.Simple ( SimplePrettyApp, runSimplePrettyApp )
import           RIO.PrettyPrint.StylesUpdate
                   ( StylesUpdate, parseStylesUpdateFromString )
import           RIO.Time ( fromGregorian )
import           System.Terminal ( hIsTerminalDeviceOrMinTTY, getTerminalWidth )

-- | Type representing options that can be specified at the command line
data Options = Options
  { colours :: String
  , theme :: Theme
  }

-- | Type representing styles identified by a theme name
data Theme
  = Default
  | SolarizedDark
  deriving (Bounded, Enum, Read, Show)

options :: Parser Options
options = Options
  <$> strOption
        (  long "colours"
        <> metavar "STYLES"
        <> help "Specify the output styles; STYLES is a colon-delimited \
                \sequence of key=value, where 'key' is a style name and \
                \'value' is a semicolon-delimited list of 'ANSI' SGR (Select \
                \Graphic Rendition) control codes (in decimal). In shells \
                \where a semicolon is a command separator, enclose STYLES in \
                \quotes."
        <> value ""
        )
  <*> option auto
        ( long "theme"
        <> metavar "THEME"
        <> help (  "Specify a theme for output styles. THEME is one of: "
                <> showThemes <> "."
                )
        <> value Default
        <> showDefault
        )
 where
  showThemes = L.intercalate " " $ map show ([minBound .. maxBound] :: [Theme])

fromTheme :: Theme -> StylesUpdate
fromTheme Default = mempty
fromTheme SolarizedDark = parseStylesUpdateFromString
  "error=31:good=32:shell=35:dir=34:recommendation=32:target=95:module=35:package-component=95:secondary=92:highlight=32"

main :: IO ()
main = do
  isTerminal <- hIsTerminalDeviceOrMinTTY stderr
  if isTerminal
    then do
      terminalWidth <- fromMaybe 80 <$> getTerminalWidth
      mainInTerminal terminalWidth =<< execParser opts
    else
      putStrLn "This executable is intended to be run with the standard error \
               \ channel connected to a terminal. No terminal detected."
 where
  opts = info (options <**> helper)
    (  fullDesc
    <> progDesc "Allows a person to inspect in a terminal the form of Pantry's \
                \pretty exceptions."
    <> header "test-pretty-exceptions - test Pantry's pretty exceptions"
    )

mainInTerminal :: Int -> Options -> IO ()
mainInTerminal terminalWidth Options{..} = do
  let stylesUpdate = fromTheme theme <> parseStylesUpdateFromString colours
  runSimplePrettyApp terminalWidth stylesUpdate action
 where
  action :: RIO SimplePrettyApp ()
  action = mapM_ (prettyError . pretty) examples

-- | The intention is that there shoud be examples for every data constructor of
-- the PantryException type.
examples :: [PantryException]
examples = concat
  [ [ PackageIdentifierRevisionParseFail hackageMsg ]
  , [ RawPackageLocationImmutableParseFail "example text" someExceptionExample ]
  , [ RawPackageLocationImmutableParseWarnings "example text" jsonWarningsExample]
  , [ InvalidCabalFile loc version pErrorExamples pWarningExamples
    | loc <- map Left rawPackageLocationImmutableExamples <> [Right pathAbsFileExample]
    , version <- [Nothing, Just versionExample]
    ]
  , [ TreeWithoutCabalFile rawPackageLocationImmutable
    | rawPackageLocationImmutable <- rawPackageLocationImmutableExamples
    ]
  , [ TreeWithMultipleCabalFiles rawPackageLocationImmutable safeFilePathExamples
    | rawPackageLocationImmutable <- rawPackageLocationImmutableExamples
    ]
  , [ MismatchedCabalName pathAbsFileExample packageNameExample ]
  , [ NoCabalFileFound pathAbsDirExample ]
  , [ MultipleCabalFilesFound pathAbsDirExample pathAbsFileExamples ]
  , [ InvalidWantedCompiler "my-wanted-compiler" ]
  , [ InvalidSnapshotLocation pathAbsDirExample rawPathExample ]
  , [ InvalidOverrideCompiler wantedCompiler1 wantedCompiler2
    | wantedCompiler1 <- wantedCompilerExamples
    , wantedCompiler2 <- wantedCompilerExamples
    ]
  , [ InvalidFilePathSnapshot rawPathExample ]
  , [ InvalidSnapshot rawSnapshotLocation someExceptionExample
    | rawSnapshotLocation <- rawSnapshotLocationExamples
    ]
  , [ InvalidGlobalHintsLocation pathAbsDirExample rawPathExample ]
  , [ InvalidFilePathGlobalHints rawPathExample ]
  , [ MismatchedPackageMetadata rawPackageLocationImmutable rawPackageMetadata treeKey packageIdentifierExample
    | rawPackageLocationImmutable <- rawPackageLocationImmutableExamples
    , rawPackageMetadata <- rawPackageMetadataExamples
    , treeKey <- [Nothing, Just treeKeyExample]
    ]
  , [ Non200ResponseStatus statusExample ]
  , [ InvalidBlobKey (Mismatch blobKeyExample blobKeyExample) ]
  , [ Couldn'tParseSnapshot rawSnapshotLocation errorMessageExample
    | rawSnapshotLocation <- rawSnapshotLocationExamples
    ]
  , [ WrongCabalFileName rawPackageLocationImmutable safeFilePathExample packageNameExample
    | rawPackageLocationImmutable <- rawPackageLocationImmutableExamples
    ]
  , [ DownloadInvalidSHA256 urlExample (Mismatch sha256Example sha256Example) ]
  , [ DownloadInvalidSize urlExample (Mismatch fileSizeExample fileSizeExample) ]
  , [ DownloadTooLarge urlExample (Mismatch fileSizeExample fileSizeExample) ]
  , [ LocalNoArchiveFileFound pathAbsFileExample ]
  , [ LocalInvalidSHA256 pathAbsFileExample (Mismatch sha256Example sha256Example) ]
  , [ LocalInvalidSize pathAbsFileExample (Mismatch fileSizeExample fileSizeExample) ]
  , [ UnknownArchiveType archiveLocation
    | archiveLocation <- archiveLocationExamples
    ]
  , [ InvalidTarFileType archiveLocation filePathExample fileTypeExample
    | archiveLocation <- archiveLocationExamples
    ]
  , [ UnsupportedTarball archiveLocation (T.pack errorMessageExample)
    | archiveLocation <- archiveLocationExamples
    ]
  , [ NoHackageCryptographicHash packageIdentifierExample ]
  , [ FailedToCloneRepo simpleRepoExample ]
  , [ TreeReferencesMissingBlob rawPackageLocationImmutable safeFilePathExample blobKeyExample
    | rawPackageLocationImmutable <- rawPackageLocationImmutableExamples
    ]
  , [ CompletePackageMetadataMismatch rawPackageLocationImmutable packageMetadataExample
    | rawPackageLocationImmutable <- rawPackageLocationImmutableExamples
    ]
  , [ CRC32Mismatch archiveLocation filePathExample (Mismatch 1024 1024 )
    | archiveLocation <- archiveLocationExamples
    ]
  , [ UnknownHackagePackage packageIdentifierRevisionExample fuzzyResults
    | packageIdentifierRevisionExample <- packageIdentifierRevisionExamples
    , fuzzyResults <- fuzzyResultsExamples
    ]
  , [ CannotCompleteRepoNonSHA1 repoExample ]
  , [ MutablePackageLocationFromUrl urlExample ]
  , [ MismatchedCabalFileForHackage packageIdentifierRevision (Mismatch packageIdentifierExample packageIdentifierExample)
    | packageIdentifierRevision <- packageIdentifierRevisionExamples
    ]
  , [ PackageNameParseFail rawPackageName ]
  , [ PackageVersionParseFail rawPackageVersion ]
  , [ InvalidCabalFilePath pathAbsFileExample ]
  , [ DuplicatePackageNames sourceMsgExample duplicatePackageNamesExamples ]
  , [ MigrationFailure descriptionExample pathAbsFileExample someExceptionExample ]
  , [ NoCasaConfig ]
  , [ InvalidTreeFromCasa blobKeyExample blobExample ]
  , [ ParseSnapNameException rawSnapNameExample ]
  , [ HpackLibraryException pathAbsFileExample errorMessageExample ]
  , [ HpackExeException hpackCommandExample pathAbsDirExample someExceptionExample ]
  ]

hackageMsg :: Text
hackageMsg = "<Example message from Hackage.>"

pErrorExamples :: [C.PError]
pErrorExamples =
  [ C.PError (C.Position 10 20) "<Example error message1.>"
  , C.PError (C.Position 12 10) "<Example error message2.>"
  , C.PError (C.Position 14 30) "<Example error message3.>"
  ]

pWarningExamples :: [C.PWarning]
pWarningExamples =
  [ C.PWarning C.PWTOther (C.Position 10 20) "<Example warning message1.>"
  , C.PWarning C.PWTOther (C.Position 12 10) "<Example warning message2.>"
  , C.PWarning C.PWTOther (C.Position 14 30) "<Example warning message3.>"
  ]

packageNameExample :: PackageName
packageNameExample = C.mkPackageName "my-package"

versionExample :: Version
versionExample = C.mkVersion [1, 0, 0]

sha256Example :: SHA256
sha256Example = hashBytes "example"

fileSizeExample :: FileSize
fileSizeExample = FileSize 1234

revisionExample :: Revision
revisionExample = Revision 1

cabalFileInfoExamples :: [CabalFileInfo]
cabalFileInfoExamples = concat
  [ [CFILatest]
  , [ CFIHash sha256Example fileSize
    | fileSize <- [Nothing, Just fileSizeExample]
    ]
  , [CFIRevision revisionExample]
  ]

packageIdentifierRevisionExamples :: [PackageIdentifierRevision]
packageIdentifierRevisionExamples =
  [ PackageIdentifierRevision packageNameExample versionExample cabalFileInfo
  | cabalFileInfo <- cabalFileInfoExamples
  ]

blobKeyExample :: BlobKey
blobKeyExample = BlobKey sha256Example fileSizeExample

treeKeyExample :: TreeKey
treeKeyExample = TreeKey blobKeyExample

rawPackageLocationImmutableExamples :: [RawPackageLocationImmutable]
rawPackageLocationImmutableExamples =
     [ RPLIHackage packageIdentifierRevision treeKey
     | packageIdentifierRevision <- packageIdentifierRevisionExamples
     , treeKey <- [Nothing, Just treeKeyExample]
     ]
--, RPLIArchive
  <> [ RPLIRepo repoExample rawPackageMetadata
     | rawPackageMetadata <- rawPackageMetadataExamples
     ]

safeFilePathExamples :: [SafeFilePath]
safeFilePathExamples =
  [ fromJust $ mkSafeFilePath "Users/jane/my-project-dir/example1.ext"
  , fromJust $ mkSafeFilePath "Users/jane/my-project-dir/example2.ext"
  , fromJust $ mkSafeFilePath "Users/jane/my-project-dir/example3.ext"
  ]

rawPathExample :: Text
rawPathExample = "<Example raw path.>"

wantedCompilerExamples :: [WantedCompiler]
wantedCompilerExamples =
  [ WCGhc versionExample
  , WCGhcGit "<commit1>" "<commit2>"
  , WCGhcjs versionExample versionExample
  ]

newtype ExceptionExample
  = ExceptionExample Text
  deriving (Show, Typeable)

instance Exception ExceptionExample where
  displayException (ExceptionExample t) = T.unpack t

errorMessageExample :: String
errorMessageExample =
  "This is the first line of some example text for the message in an exception \
  \example. This is example text for an exception example.\n\
  \This is the second line of some example text for the message in an exception \
  \example. This is example text for an exception example."

someExceptionExample :: SomeException
someExceptionExample =
  SomeException (ExceptionExample $ T.pack errorMessageExample)

urlExample :: Text
urlExample = "https://example.com"

relFilePathExample :: RelFilePath
relFilePathExample = RelFilePath "jane/my-project-dir"

resolvedPathFileExample :: ResolvedPath File
resolvedPathFileExample = ResolvedPath relFilePathExample pathAbsFileExample

snapNameExamples :: [SnapName]
snapNameExamples =
  [ LTS 20 17
  , Nightly $ fromGregorian 2023 4 5
  ]

rawSnapshotLocationExamples :: [RawSnapshotLocation]
rawSnapshotLocationExamples = concat
  [ [ RSLCompiler wantedCompiler
    | wantedCompiler <- wantedCompilerExamples
    ]
  , [ RSLUrl urlExample blobKey
    | blobKey <- [Nothing, Just blobKeyExample]
    ]
  , [ RSLFilePath resolvedPathFileExample ]
  , [ RSLSynonym snapNameExample
    | snapNameExample <- snapNameExamples
    ]
  ]

rawPackageMetadataExamples :: [RawPackageMetadata]
rawPackageMetadataExamples =
  [ RawPackageMetadata name version treeKey
  | name <- [ Nothing, Just packageNameExample]
  , version <- [ Nothing, Just versionExample ]
  , treeKey <- [Nothing, Just treeKeyExample]
  ]

statusExample :: Status
statusExample = mkStatus 100 "<Example status message.>"

safeFilePathExample :: SafeFilePath
safeFilePathExample =
  fromJust $ mkSafeFilePath "Users/jane/my-project-dir/example.ext"

archiveLocationExamples :: [ArchiveLocation]
archiveLocationExamples =
  [ ALUrl urlExample
  , ALFilePath resolvedPathFileExample
  ]

filePathExample :: FilePath
filePathExample = "<file-path>"

fileTypeExample :: Tar.FileType
fileTypeExample = Tar.FTNormal

commitExample :: Text
commitExample = "b8b34bf5571de75909d97f687e3d37909b1dc9f7"

simpleRepoExample :: SimpleRepo
simpleRepoExample = SimpleRepo urlExample commitExample RepoGit

packageIdentifierExample :: PackageIdentifier
packageIdentifierExample = PackageIdentifier packageNameExample versionExample

packageMetadataExample :: PackageMetadata
packageMetadataExample = PackageMetadata packageIdentifierExample treeKeyExample

fuzzyResultsExamples :: [FuzzyResults]
fuzzyResultsExamples =
  [ FRNameNotFound packageNameExamples
  , FRVersionNotFound $ fromJust $ nonEmpty packageIdentifierRevisionExamples
  , FRRevisionNotFound $ fromJust $ nonEmpty packageIdentifierRevisionExamples
  ]

repoExample :: Repo
repoExample = Repo urlExample commitExample RepoGit "my-subdirectory"

rawPackageName :: Text
rawPackageName = "<raw-package-name>"

rawPackageVersion :: Text
rawPackageVersion = "<raw-package-version>"

sourceMsgExample :: Utf8Builder
sourceMsgExample = "<Example source message.>"

packageNameExamples :: [PackageName]
packageNameExamples =
  [ C.mkPackageName "my-package1"
  , C.mkPackageName "my-package2"
  , C.mkPackageName "my-package3"
  ]

duplicatePackageNamesExamples :: [(PackageName, [RawPackageLocationImmutable])]
duplicatePackageNamesExamples = map
  (, rawPackageLocationImmutableExamples)
  packageNameExamples

descriptionExample :: Text
descriptionExample = "<Example description.>"

blobExample :: ByteString
blobExample = "b8b34bf5571de75909d97f687e3d37909b1dc9f7"

rawSnapNameExample :: Text
rawSnapNameExample = "<raw-snapshot-name>"

hpackCommandExample :: FilePath
hpackCommandExample = "<path-to-hpack>/hpack"

jsonWarningsExample :: [JSONWarning]
jsonWarningsExample =
  [ jsonUnrecognizedFieldsExample
  , jsonGeneralWarningExample
  ]

jsonUnrecognizedFieldsExample :: JSONWarning
jsonUnrecognizedFieldsExample = JSONUnrecognizedFields
  "UnresolvedPackageLocationImmutable.UPLIHackage"
  ["field1", "field2", "field3"]

jsonGeneralWarningExample :: JSONWarning
jsonGeneralWarningExample = JSONGeneralWarning "A general JSON warning."
