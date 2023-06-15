{-# LANGUAGE OverloadedStrings #-}

-- | Exposed for testing, do not use!
module Pantry.Internal
  ( parseTree
  , renderTree
  , Tree (..)
  , TreeEntry (..)
  , FileType(..)
  , mkSafeFilePath
  , pcHpackExecutable
  , normalizeParents
  , makeTarRelative
  , getGlobalHintsFile
  , hpackVersion
  , Storage
  , initStorage
  , withStorage_
  ) where

import           Control.Exception ( assert )
import           Data.Maybe ( fromMaybe )
import qualified Data.Text as T
import           Pantry.HPack ( hpackVersion )
import           Pantry.SQLite ( initStorage )
import           Pantry.Types

-- | Like @System.FilePath.normalise@, however:
--
-- * Only works on relative paths, absolute paths fail
--
-- * Strips trailing slashes
--
-- * Only works on forward slashes, even on Windows
--
-- * Normalizes parent dirs @foo/../@ get stripped
--
-- * Cannot begin with a parent directory (@../@)
--
-- * Spelled like an American, sorry
normalizeParents ::
     FilePath
  -> Either String FilePath
normalizeParents "" = Left "empty file path"
normalizeParents ('/':_) = Left "absolute path"
normalizeParents ('.':'.':'/':_) = Left "absolute path"
normalizeParents fp = do
  -- Strip a single trailing, but not multiple
  let t0 = T.pack fp
      t = fromMaybe t0 $ T.stripSuffix "/" t0
  case T.unsnoc t of
    Just (_, '/') -> Left "multiple trailing slashes"
    _ -> Right ()

  let c1 = T.split (== '/') t

  case reverse c1 of
    ".":_ -> Left "last component is a single dot"
    _ -> Right ()

  let c2 = filter (\x -> not (T.null x || x == ".")) c1

  let loop [] routput = reverse routput
      loop ("..":rest) (_:routput) = loop rest routput
      loop (x:xs) routput = loop xs (x:routput)

  case loop c2 [] of
    [] -> Left "no non-empty components"
    c' -> Right $ T.unpack $ T.intercalate "/" c'

-- | Following tar file rules (Unix file paths only), make the second file
-- relative to the first file.
makeTarRelative ::
     FilePath -- ^ base file
  -> FilePath -- ^ relative part
  -> Either String FilePath
makeTarRelative _ ('/':_) = Left "absolute path found"
makeTarRelative base rel =
  case reverse base of
    [] -> Left "cannot have empty base"
    '/':_ -> Left "base cannot be a directory"
    _:rest -> Right $
      case dropWhile (/= '/') rest of
        '/':rest' -> reverse rest' ++ '/' : rel
        rest' -> assert (null rest') rel
