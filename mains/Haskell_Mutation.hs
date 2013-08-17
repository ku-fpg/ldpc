module Main where

import ECC
import Haskell.ArraySig
import Codes.ICFP_Paper
import Manifold.Haskell

main :: IO ()
main = do
  putStrLn "Start"
  let h = fromListMatrix Codes.ICFP_Paper.h_4096_7168
  h `seq` putStrLn "Evaluated h."
  g <- Codes.ICFP_Paper.load_g_4096_7168
  g `seq` putStrLn "Evaluated g."
  ecc <- ecc_mutation 25 h g (Just 3) 1024
  mainWith 1 $ ecc{debug=noDebug}
