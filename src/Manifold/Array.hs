{-# LANGUAGE ScopedTypeVariables, DataKinds #-}
module Manifold.Array where

import Data.Bit
import LDPC.Array.Sig as S
import Data.Array
import qualified LDPC.Array.Encode as E
import qualified LDPC.Array.Decode as D
import Data.BitMatrix.Sparse

import Data.Alist
import Codes.MoonLDPC

import ECC

-- This is the first model of LDPC

array_ecc' :: String -> Int -> IO ECC
array_ecc' codename maxIterations = do
    g :: BitMatrix CRS <- readAlist ("codes/" ++ codename ++ ".G")
    h :: BitMatrix CRS <- readAlist ("codes/" ++ codename ++ ".H")
    let (n,m) = sizeBM g
    return $ defaultECC
        { encode   = return . fromVector . E.encoder (toArray g) . vector
        , decode   = return . take n . fromVector . D.decoder maxIterations (toArray h) . vector
        , message_length  = n
        , codeword_length = m
        }

toArray :: BitMatrix 'CRS -> M Bit
toArray a = array ((1,1),sizeBM a) [ ((n,m),a `idx` (n,m)) | (n,m) <- range ((1,1),sizeBM a) ]
{-
array_ecc g h maxIterations = defaultECC
        { encode   = return . fromVector . E.encoder g . vector
        , decode   = return . take 7 . fromVector . D.decoder maxIterations moon_array_h . vector
        , message_length  = 7
        , codeword_length = 20
        }
-}

{-
min_array_ecc :: Int -> ECC -> ECC
min_array_ecc maxIterations ecc = ecc
        { decode   = return . take (message_length ecc) . fromVector . D.min_decoder maxIterations moon_array_h . vector
        }
-}
---------------------------------------------------------
{-
moon_array_g :: M Bit
moon_array_g = S.matrix g_7_20

moon_array_h :: M Bit
moon_array_h = S.matrix h_7_20

moon_array_ecc :: Int -> ECC
moon_array_ecc = array_ecc moon_array_g moon_array_h

min_moon_array_ecc :: Int -> ECC
min_moon_array_ecc mx = min_array_ecc mx $ array_ecc moon_array_g moon_array_h mx
-}