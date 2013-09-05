module Main where

import ECC

import qualified Manifold.Array as A

pickECC :: String -> ECC
pickECC "array-moon:200" = A.moon_array_ecc 200

main = do
        let ecc0 = pickECC "array-moon:200"
        let ecc1 = txRx_EbN0 3.0 ecc0
        runECC (Just 0) 100 ecc1 { verbose = 0 }

