module Haskell where

import ECC
import Noise

import Haskell.ArraySig
import Data.Array.Base (listArray,elems,bounds)
import Data.Array (rangeSize)
import Haskell.Encode (encoder)
import Haskell.Decode (decoder_mutation)

import System.Random.MWC (create)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans (MonadIO(..))

import Control.Applicative ((<$>))

ecc_mutation :: (Functor m,PrimMonad m,MonadIO m) =>
  Int -> M Bool -> M Bool -> Noisyness -> Int -> m (ECC m V V Double Int)
ecc_mutation maxIterations h g noisyness untxBits =
  create >>= \gen -> return $ ECC
  { generate = listArray (1,originalBits) <$> generateList gen originalBits
  , encode = return . encoder g
    -- do not transmit untxBits parity bits
  , txRx = fmap (listArray (1,allBits))
         . fmap (++ replicate untxBits 0)
         . addNoise gen rate noisyness
         . take frameSize
         . elems
  , decode = return . decoder_mutation maxIterations h
  , check = \x y -> checkList (elems x) (elems y)
  , ber = berForFramesize frameSize
  , debug = liftIO . putStrLn
  , showX = \i -> Just $ "iterations " ++ show i
  , showV = map (\b -> if b then '1' else '0') . elems
  , showW = show . elems
  , showWBool = map (\b -> if b then '1' else '0') . elems
  }
  where rate = packet_size / punctured_size
          where packet_size    = fromIntegral originalBits
                punctured_size = fromIntegral $ allBits-untxBits

        -- NB do not use h here, it may be truncated
        ((rBase,cBase),(rTop,cTop)) = bounds g
        numRows = rangeSize (rBase,rTop)
        numCols = rangeSize (cBase,cTop)

        -- the bits being encoded
        allBits      = numCols
        originalBits = numRows
        _parityBits   = allBits-originalBits

        -- the number of bits transmitted
        frameSize = allBits-untxBits
