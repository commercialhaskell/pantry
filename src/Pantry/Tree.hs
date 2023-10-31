{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns      #-}

module Pantry.Tree
  ( unpackTree
  , rawParseGPD
  ) where

import           Distribution.PackageDescription ( GenericPackageDescription )
import           Distribution.PackageDescription.Parsec
                   ( parseGenericPackageDescription, runParseResult )
import           Distribution.Parsec ( PWarning (..) )
import           Pantry.Storage ( loadBlob, withStorage )
import           Pantry.Types
                   ( FileType (..), HasPantryConfig, PantryException (..)
                   , RawPackageLocationImmutable, Tree (..), TreeEntry (..)
                   , unSafeFilePath
                   )
import           Path ( Abs, Dir, File, Path, toFilePath )
import           RIO
import qualified RIO.ByteString as B
import           RIO.Directory
                   ( createDirectoryIfMissing, getPermissions
                   , setOwnerExecutable, setPermissions
                   )
import           RIO.FilePath ((</>), takeDirectory)
import qualified RIO.Map as Map
import qualified RIO.Text as T

unpackTree ::
     (HasPantryConfig env, HasLogFunc env)
  => RawPackageLocationImmutable -- for exceptions
  -> Path Abs Dir -- ^ dest dir, will be created if necessary
  -> Tree
  -> RIO env ()
unpackTree rpli (toFilePath -> dir) (TreeMap m) = do
  withStorage $ for_ (Map.toList m) $ \(sfp, TreeEntry blobKey ft) -> do
    let dest = dir </> T.unpack (unSafeFilePath sfp)
    createDirectoryIfMissing True $ takeDirectory dest
    mbs <- loadBlob blobKey
    case mbs of
      Nothing -> do
        -- TODO when we have pantry wire stuff, try downloading
        throwIO $ TreeReferencesMissingBlob rpli sfp blobKey
      Just bs -> do
        B.writeFile dest bs
        case ft of
          FTNormal -> pure ()
          FTExecutable -> liftIO $ do
            perms <- getPermissions dest
            setPermissions dest $ setOwnerExecutable True perms

-- | A helper function that performs the basic character encoding necessary.
rawParseGPD ::
     MonadThrow m
  => Either RawPackageLocationImmutable (Path Abs File)
  -> ByteString
  -> m ([PWarning], GenericPackageDescription)
rawParseGPD loc bs =
  case eres of
    Left (mversion, errs) ->
      throwM $ InvalidCabalFile loc mversion (toList errs) warnings
    Right gpkg -> pure (warnings, gpkg)
 where
  (warnings, eres) = runParseResult $ parseGenericPackageDescription bs
