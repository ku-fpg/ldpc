module Main where

import ECC
import Haskell
import Codes.ICFP_Paper
import Haskell.ArraySig

main :: IO ()
main = mainWith 100 $ (\ecc -> ecc{debug=noDebug}) `fmap` ecc_mutation 25 (fromListMatrix Codes.ICFP_Paper.h_4096_7168) Codes.ICFP_Paper.g_4096_7168 (Just 3) 1024
--main = mainWith $ ecc_mutation 20 (fromListMatrix Codes.ICFP_Paper.h_7_20) (fromListMatrix Codes.ICFP_Paper.g_7_20) (Just 4) 0
