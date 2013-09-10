module Stats (nintyFifth,  Estimate(..)) where

import qualified Data.Vector.Unboxed as U
import Statistics.Sample (mean)
import Statistics.Resampling (resample, fromResample)
import Statistics.Resampling.Bootstrap (bootstrapBCA, Estimate(..) )
import System.Random.MWC (create)

nintyFifth :: [Double] -> IO Estimate
nintyFifth sample = do
          g <- create
          resamples <- resample g [mean] 10000 sampleU -- (length sample^2) sampleU
--          print $ U.length $ fromResample $ head $ resamples
--          print resamples
          return $ head $ bootstrapBCA 0.95 sampleU [mean] resamples
  where
    sampleU = U.fromList sample
