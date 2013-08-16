-- this is copied from the hfec repo, changed to operate on lists, and updated
-- for the current MWC interface

-- I'm not sure what LC is for; the old code only used it with atanh/tanh

module Noise where

import System.Random.MWC  as MWC
import System.Random.MWC.Distributions (standard)
import Control.Monad.Primitive

import Control.Monad (replicateM)

type Noisyness = Maybe Double
type Rate = Double

addNoise :: PrimMonad m =>
  Gen (PrimState m) -> Rate -> Noisyness -> [Bool] -> m [Double]
addNoise gen rate noisyness = case noisyness of
  Nothing -> return . map antip
  Just i  -> addNoise' gen rate (getSigma2 i)

addNoise' :: PrimMonad m =>
  Gen (PrimState m) -> Double -> Double -> [Bool] -> m [Double]
addNoise' gen ec sigma2 v = do
  let sgen = standard gen -- a normal distribution

  adjustments <- replicateM (length v) sgen

  return $ zipWith (+)
    (fmap ((* sqrt ec) . antip) v)
    (fmap (* sqrt sigma2) adjustments)

getSigma2 ::  Double -> Double
getSigma2 ebnoDB = ((1/10) ** (ebnoDB/10)) / 2

--getLC :: Double -> Double -> Double
--getLC ec sigma2 = 2 * sqrt(ec) / sigma2

antip :: Bool -> Double
antip  bval | bval = 1.0
            | otherwise =  -1.0
