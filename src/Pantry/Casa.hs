{-# LANGUAGE DisambiguateRecordFields #-}

-- | Integration with the Casa server.

module Pantry.Casa where

import qualified Casa.Client as Casa
import qualified Casa.Types as Casa
import           Conduit
import qualified Data.HashMap.Strict as HM
import qualified Pantry.SHA256 as SHA256
import           Pantry.Storage hiding ( findOrGenerateCabalFile )
import           Pantry.Types as P
import           RIO
import qualified RIO.ByteString as B

-- | Lookup a tree.
casaLookupTree ::
     (HasPantryConfig env, HasLogFunc env)
  => TreeKey
  -> RIO env (Maybe (TreeKey, P.Tree))
casaLookupTree (P.TreeKey key) =
  handleAny (const (pure Nothing))
    (withStorage
      (runConduitRes (casaBlobSource (Identity key) .| mapMC parseTreeM .| await)))

-- | Lookup a single blob. If possible, prefer 'casaBlobSource', and
-- query a group of keys at once, rather than one at a time. This will
-- have better network performance.
casaLookupKey ::
     (HasPantryConfig env, HasLogFunc env)
  => BlobKey
  -> RIO env (Maybe ByteString)
casaLookupKey key =
  handleAny (const (pure Nothing))
  (fmap
    (fmap snd)
    (withStorage (runConduitRes (casaBlobSource (Identity key) .| await))))

-- | A source of blobs given a set of keys. All blobs are
-- automatically stored in the local pantry database.
casaBlobSource ::
     (Foldable f, HasPantryConfig env, HasLogFunc env)
  => f BlobKey
  -> ConduitT i (BlobKey, ByteString) (ResourceT (ReaderT SqlBackend (RIO env))) ()
casaBlobSource keys = source .| convert .| store
  where
    source = do
      pullUrl <- lift $ lift $ lift $ view $ pantryConfigL . to pcCasaRepoPrefix
      maxPerRequest <- lift $ lift $ lift $ view $ pantryConfigL . to pcCasaMaxPerRequest
      Casa.blobsSource
        (Casa.SourceConfig
           { sourceConfigUrl = pullUrl
           , sourceConfigBlobs = toBlobKeyMap keys
           , sourceConfigMaxBlobsPerRequest = maxPerRequest
           })
      where
        toBlobKeyMap :: Foldable f => f BlobKey -> HashMap Casa.BlobKey Int
        toBlobKeyMap = HM.fromList . map unpackBlobKey . toList
        unpackBlobKey (P.BlobKey sha256 (FileSize fileSize)) =
          (Casa.BlobKey (SHA256.toRaw sha256), fromIntegral fileSize)
    convert = mapMC toBlobKeyAndBlob
      where
        toBlobKeyAndBlob ::
             MonadThrow m
          => (Casa.BlobKey, ByteString)
          -> m (BlobKey, ByteString)
        toBlobKeyAndBlob (Casa.BlobKey keyBytes, blob) = do
          sha256 <-
            case SHA256.fromRaw keyBytes of
              Left e -> throwM e
              Right sha -> pure sha
          pure (BlobKey sha256 (FileSize (fromIntegral (B.length blob))), blob)
    store = mapMC insertBlob
      where
        insertBlob original@(_key, binary) = do
          _ <- lift (storeBlob binary)
          pure original
