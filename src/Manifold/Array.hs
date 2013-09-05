module Manifold.Array where

import Data.Bit
import Haskell.Array.Sig as S
import qualified Haskell.Array.Encode as E
--import qualified Haskell.Array.Decode as D

import Codes.MoonLDPC

import ECC

-- This is the first model of LDPC

moon_array_g :: M Bit
moon_array_g = S.matrix g_7_20

moon_array_h :: M Bit
moon_array_h = S.matrix h_7_20

moon_array_ecc :: Int -> ECC
moon_array_ecc maxIterations = defaultECC
        { encode   = return . E.encoder moon_array_g
        , decode   = return . fmap setBit . fmap (>= 0) . take 7
        , message_length  = 7
        , codeword_length = 20
        }

