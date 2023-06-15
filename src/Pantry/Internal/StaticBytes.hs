{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | This is an unstable API, exposed only for testing. Relying on
-- this may break your code! Caveat emptor.
--
-- This module can (and perhaps should) be separate into its own
-- package, it's generally useful.
module Pantry.Internal.StaticBytes
  ( Bytes8
  , Bytes16
  , Bytes32
  , Bytes64
  , Bytes128
  , DynamicBytes
  , StaticBytes
  , StaticBytesException (..)
  , toStaticExact
  , toStaticPad
  , toStaticTruncate
  , toStaticPadTruncate
  , fromStatic
  ) where

import           Data.Bits
import           Data.ByteArray
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.Primitive.ByteArray as BA
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Base as VU
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           RIO hiding ( words )
import           System.IO.Unsafe ( unsafePerformIO )

newtype Bytes8 = Bytes8 Word64
  deriving (Eq, Ord, Generic, NFData, Hashable, Data)

instance Show Bytes8 where
  show (Bytes8 w) = show (fromWordsD 8 [w] :: B.ByteString)

data Bytes16 = Bytes16 !Bytes8 !Bytes8
  deriving (Show, Eq, Ord, Generic, NFData, Hashable, Data)

data Bytes32 = Bytes32 !Bytes16 !Bytes16
  deriving (Show, Eq, Ord, Generic, NFData, Hashable, Data)

data Bytes64 = Bytes64 !Bytes32 !Bytes32
  deriving (Show, Eq, Ord, Generic, NFData, Hashable, Data)

data Bytes128 = Bytes128 !Bytes64 !Bytes64
  deriving (Show, Eq, Ord, Generic, NFData, Hashable, Data)

data StaticBytesException
  = NotEnoughBytes
  | TooManyBytes
  deriving (Show, Eq, Typeable)

instance Exception StaticBytesException

-- All lengths below are given in bytes

class DynamicBytes dbytes where
  lengthD :: dbytes -> Int
  -- | Yeah, it looks terrible to use a list here, but fusion should kick in
  withPeekD :: dbytes -> ((Int -> IO Word64) -> IO a) -> IO a
  -- | May throw a runtime exception if invariants are violated!
  fromWordsD :: Int -> [Word64] -> dbytes

fromWordsForeign ::
     (ForeignPtr a -> Int -> b)
  -> Int
  -> [Word64]
  -> b
fromWordsForeign wrapper len words0 = unsafePerformIO $ do
  fptr <- B.mallocByteString len
  withForeignPtr fptr $ \ptr -> do
    let loop _ [] = pure ()
        loop off (w:ws) = do
          pokeElemOff (castPtr ptr) off w
          loop (off + 1) ws
    loop 0 words0
  pure $ wrapper fptr len

withPeekForeign ::
     (ForeignPtr a, Int, Int)
  -> ((Int -> IO Word64) -> IO b)
  -> IO b
withPeekForeign (fptr, off, len) inner =
  withForeignPtr fptr $ \ptr -> do
    let f off'
          | off' >= len = pure 0
          | off' + 8 > len = do
              let loop w64 i
                    | off' + i >= len = pure w64
                    | otherwise = do
                        w8 :: Word8 <- peekByteOff ptr (off + off' + i)
                        let w64' = shiftL (fromIntegral w8) (i * 8) .|. w64
                        loop w64' (i + 1)
              loop 0 0
          | otherwise = peekByteOff ptr (off + off')
    inner f

instance DynamicBytes B.ByteString where
  lengthD = B.length
  fromWordsD = fromWordsForeign (`B.fromForeignPtr` 0)
  withPeekD = withPeekForeign . B.toForeignPtr

instance word8 ~ Word8 => DynamicBytes (VS.Vector word8) where
  lengthD = VS.length
  fromWordsD = fromWordsForeign VS.unsafeFromForeignPtr0
  withPeekD = withPeekForeign . VS.unsafeToForeignPtr

instance word8 ~ Word8 => DynamicBytes (VP.Vector word8) where
  lengthD = VP.length
  fromWordsD len words0 = unsafePerformIO $ do
    ba <- BA.newByteArray len
    let loop _ [] =
          VP.Vector 0 len <$> BA.unsafeFreezeByteArray ba
        loop i (w:ws) = do
          BA.writeByteArray ba i w
          loop (i + 1) ws
    loop 0 words0
  withPeekD (VP.Vector off len ba) inner = do
    let f off'
          | off' >= len = pure 0
          | off' + 8 > len = do
              let loop w64 i
                    | off' + i >= len = pure w64
                    | otherwise = do
                        let w8 :: Word8 = BA.indexByteArray ba (off + off' + i)
                        let w64' = shiftL (fromIntegral w8) (i * 8) .|. w64
                        loop w64' (i + 1)
              loop 0 0
          | otherwise = pure $ BA.indexByteArray ba (off + (off' `div` 8))
    inner f

instance word8 ~ Word8 => DynamicBytes (VU.Vector word8) where
  lengthD = VU.length
  fromWordsD len words = VU.V_Word8 (fromWordsD len words)
  withPeekD (VU.V_Word8 v) = withPeekD v

class StaticBytes sbytes where
  lengthS :: proxy sbytes -> Int -- use type level literals instead?
  -- difference list
  toWordsS :: sbytes -> [Word64] -> [Word64]
  usePeekS :: Int -> (Int -> IO Word64) -> IO sbytes

instance StaticBytes Bytes8 where
  lengthS _ = 8
  toWordsS (Bytes8 w) = (w:)
  usePeekS off f = Bytes8 <$> f off

instance StaticBytes Bytes16 where
  lengthS _ = 16
  toWordsS (Bytes16 b1 b2) = toWordsS b1 . toWordsS b2
  usePeekS off f = Bytes16 <$> usePeekS off f <*> usePeekS (off + 8) f

instance StaticBytes Bytes32 where
  lengthS _ = 32
  toWordsS (Bytes32 b1 b2) = toWordsS b1 . toWordsS b2
  usePeekS off f = Bytes32 <$> usePeekS off f <*> usePeekS (off + 16) f

instance StaticBytes Bytes64 where
  lengthS _ = 64
  toWordsS (Bytes64 b1 b2) = toWordsS b1 . toWordsS b2
  usePeekS off f = Bytes64 <$> usePeekS off f <*> usePeekS (off + 32) f

instance StaticBytes Bytes128 where
  lengthS _ = 128
  toWordsS (Bytes128 b1 b2) = toWordsS b1 . toWordsS b2
  usePeekS off f = Bytes128 <$> usePeekS off f <*> usePeekS (off + 64) f

instance ByteArrayAccess Bytes8 where
  length _ = 8
  withByteArray = withByteArrayS

instance ByteArrayAccess Bytes16 where
  length _ = 16
  withByteArray = withByteArrayS

instance ByteArrayAccess Bytes32 where
  length _ = 32
  withByteArray = withByteArrayS

instance ByteArrayAccess Bytes64 where
  length _ = 64
  withByteArray = withByteArrayS

instance ByteArrayAccess Bytes128 where
  length _ = 128
  withByteArray = withByteArrayS

withByteArrayS :: StaticBytes sbytes => sbytes -> (Ptr p -> IO a) -> IO a
withByteArrayS sbytes = withByteArray (fromStatic sbytes :: ByteString)

toStaticExact ::
     forall dbytes sbytes. (DynamicBytes dbytes, StaticBytes sbytes)
  => dbytes
  -> Either StaticBytesException sbytes
toStaticExact dbytes =
  case compare (lengthD dbytes) (lengthS (Nothing :: Maybe sbytes)) of
    LT -> Left NotEnoughBytes
    GT -> Left TooManyBytes
    EQ -> Right (toStaticPadTruncate dbytes)

toStaticPad ::
     forall dbytes sbytes. (DynamicBytes dbytes, StaticBytes sbytes)
  => dbytes
  -> Either StaticBytesException sbytes
toStaticPad dbytes =
  case compare (lengthD dbytes) (lengthS (Nothing :: Maybe sbytes)) of
    GT -> Left TooManyBytes
    _  -> Right (toStaticPadTruncate dbytes)

toStaticTruncate ::
     forall dbytes sbytes. (DynamicBytes dbytes, StaticBytes sbytes)
  => dbytes
  -> Either StaticBytesException sbytes
toStaticTruncate dbytes =
  case compare (lengthD dbytes) (lengthS (Nothing :: Maybe sbytes)) of
    LT -> Left NotEnoughBytes
    _  -> Right (toStaticPadTruncate dbytes)

toStaticPadTruncate ::
     (DynamicBytes dbytes, StaticBytes sbytes)
  => dbytes
  -> sbytes
toStaticPadTruncate dbytes = unsafePerformIO (withPeekD dbytes (usePeekS 0))

fromStatic ::
     forall dbytes sbytes. (DynamicBytes dbytes, StaticBytes sbytes)
  => sbytes
  -> dbytes
fromStatic = fromWordsD (lengthS (Nothing :: Maybe sbytes)) . ($ []) . toWordsS
