module Manifold.Array where

import Data.Bit
import Haskell.Array.Sig as S
import qualified Haskell.Array.Encode as E
import qualified Haskell.Array.Decode as D

import Data.Alist
import Codes.MoonLDPC

import ECC

-- This is the first model of LDPC

array_ecc' :: String -> Int -> IO ECC
array_ecc' codename maxIterations = do
    aList <- readAlist ("codes/" ++ codename)
    let h = mk_h aList
    let g = mk_g aList
    let (n,m) = nm aList
    print (ShowM h)
    print (ShowM g)
    print (n,m,n+m)
    return $ defaultECC
        { encode   = return . fromVector . E.encoder g . vector
        , decode   = return . take 7 . fromVector . D.decoder maxIterations h . vector
        , message_length  = n
        , codeword_length = n + m
        }


array_ecc g h maxIterations = defaultECC
        { encode   = return . fromVector . E.encoder g . vector
        , decode   = return . take 7 . fromVector . D.decoder maxIterations moon_array_h . vector
        , message_length  = 7
        , codeword_length = 20
        }

min_array_ecc :: Int -> ECC -> ECC
min_array_ecc maxIterations ecc = ecc
        { decode   = return . take 7 . fromVector . D.min_decoder maxIterations moon_array_h . vector
        }

---------------------------------------------------------

moon_array_g :: M Bit
moon_array_g = S.matrix g_7_20

moon_array_h :: M Bit
moon_array_h = S.matrix h_7_20

moon_array_ecc :: Int -> ECC
moon_array_ecc = array_ecc moon_array_g moon_array_h

min_moon_array_ecc :: Int -> ECC
min_moon_array_ecc mx = min_array_ecc mx $ array_ecc moon_array_g moon_array_h mx
