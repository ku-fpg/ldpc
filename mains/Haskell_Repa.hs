module Main where

import ECC
import Codes.ICFP_Paper
import Haskell.ArraySig (fromListMatrix)
import Manifold.Repa (ecc_repa)

-- ecc_repa currently cannot handle the 4K/7K code
--
-- it takes about 3ms per decode for the 7/20 code
main :: IO ()
main = do
  putStrLn "Start"
  let h = fromListMatrix Codes.ICFP_Paper.h_7_20
  let g = fromListMatrix Codes.ICFP_Paper.g_7_20
  ecc <- ecc_repa 30 h g (Just 3) 0
  mainWith (\i x -> i + toInteger x) (0 :: Integer) 1000 $ ecc{debug=noDebug}
