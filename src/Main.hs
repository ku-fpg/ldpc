module Main where

import ECC

import qualified Manifold.Array as A

pickECC :: String -> ECC
--pickECC "array-moon:200" = A.moon_array_ecc 10

pickECC "array-moon:200" = A.moon_array_ecc 10

main = do
        print "Hello"
        let ecc0 = pickECC "array-moon:200"
        let ecc1 = txRx_EbN0 4.0 ecc0
        runECC (Just 0) 1000 ecc1 { verbose = 0 }

