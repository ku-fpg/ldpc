{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

module Data.Binary.Bytewise where

import Data.Binary
import Data.Binary.Get

import Data.Array.Unboxed
import Data.Array.Base (UArray(..))
import Control.Monad.Primitive (unsafeInlineIO)

import qualified Data.Primitive as P
import GHC.Ptr (Ptr(..))
import GHC.Prim (Int#,(==#),(+#),sizeofByteArray#)
import GHC.Types (Int(I#))

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S

newtype Bytewise a = Bytewise {unBytewise::a}

getUArray :: forall i e. (Ix i,Binary i) =>
  i -> i -> Int -> Int -> L.ByteString -> UArray i e
getUArray start stop nElem nByte lbs = unsafeInlineIO $ do
  -- pinned so that we can use its address
  mbarr <- P.newPinnedByteArray nByte

  -- fill the array from the bytestring
  --
  -- TODO endianness mismatch?
  _ <- do
    let baseAddr = P.mutableByteArrayContents mbarr
        snoc acc chunk = S.useAsCStringLen chunk $ \(Ptr addr,nChunkByte) -> do
          dest <- acc
          P.copyAddr dest (P.Addr addr) nChunkByte
          return $ P.plusAddr dest nChunkByte
    L.foldlChunks snoc (return baseAddr) lbs

  -- freeze it; TODO consequences of freezing a bytearray that's pinned?
  P.ByteArray barr <- P.unsafeFreezeByteArray mbarr
  return $ UArray start stop nElem barr

instance forall i e. (Ix i,Binary i) => Binary (Bytewise (UArray i e)) where
  get = fmap Bytewise $ do
    start <- get
    stop <- get
    nElem <- get
    nByte <- get
    lbs <- getLazyByteString (toEnum nByte)
    return $ getUArray start stop nElem nByte lbs
  put (Bytewise (UArray start stop nElem barr)) = do
    put start
    put stop
    put nElem
    let !nByte = sizeofByteArray# barr
    put (I# nByte)
    let go :: Int# -> Put
        go i | i ==# nByte = return ()
             | otherwise = putWord8 (P.indexByteArray# barr i) >> go (i +# 1#)
    go 0#
