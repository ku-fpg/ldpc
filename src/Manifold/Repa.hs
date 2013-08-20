module Manifold.Repa where

import Data.Int (Int16)

import ECC
import Noise

import Haskell.ArraySig (M)
import Data.Array.Unboxed (elems,bounds,rangeSize)
import Haskell.Repa (VU,MU,encoder,decoder_Int16)
import Data.Array.Repa (Z(..),(:.)(..))
import qualified Data.Array.Repa      as R
import qualified Data.Array.Repa.Eval as R

import System.Random.MWC (create)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans (MonadIO(..))

import Control.Applicative ((<$>))

import Manifold.Haskell (cnv16)

ecc_repa :: (Functor m,PrimMonad m,MonadIO m) =>
  Int -> M Bool -> M Bool -> Noisyness -> Int -> m (ECC m VU VU Int16 Int)
ecc_repa maxIterations h' g' noisyness untxBits =
  create >>= \gen -> return $ ECC
  { generate = R.fromList (Z :. originalBits) <$> generateList gen originalBits
  , encode = return . encoder g
    -- do not transmit untxBits parity bits
  , txRx = fmap (R.fromList (Z :. allBits))
         . fmap (map cnv16)
         . fmap (++ replicate untxBits 0)
         . addNoise gen rate noisyness
         . take frameSize
         . R.toList
  , decode = return . decoder_Int16 maxIterations h
  , check = \x y -> checkList (R.toList x) (take originalBits $ R.toList y)
  , ber = berForFramesize frameSize
  , debug = liftIO . putStrLn
  , showX = \i -> Just $ "iterations " ++ show i
  , showV = map (\b -> if b then '1' else '0') . R.toList
  , showW = show . R.toList
  , showWBool = map (\b -> if b then '1' else '0') . R.toList
  }
  where h :: MU Bool
        h = R.fromList (Z :. numRows :. numCols) (elems h')
          where ((rBase,cBase),(rTop,cTop)) = bounds h'
                numRows = rangeSize (rBase,rTop)
                numCols = rangeSize (cBase,cTop)
        g :: MU Bool
        g = R.fromList (Z :. numRows :. numCols) (elems g')

        rate = packet_size / punctured_size
          where packet_size    = fromIntegral originalBits
                punctured_size = fromIntegral $ allBits-untxBits

        -- NB do not use h here, it may be truncated
        ((rBase,cBase),(rTop,cTop)) = bounds g'
        numRows = rangeSize (rBase,rTop)
        numCols = rangeSize (cBase,cTop)

        -- the bits being encoded
        allBits      = numCols
        originalBits = numRows
        _parityBits   = allBits-originalBits

        -- the number of bits transmitted
        frameSize = allBits-untxBits
