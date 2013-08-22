{-# LANGUAGE BangPatterns #-}

module Manifold.Haskell where

import Data.Int (Int32,Int16)

import ECC
import Noise

import Haskell.ArraySig
import Data.Array.Base (listArray,elems,bounds)
import Data.Array (rangeSize)
import Haskell.Encode (encoder)
import Haskell.Decode (decoder_mutation)

import qualified Haskell.DecodeCRS as CRS
import Data.Matrix.CRS

import System.Random.MWC (create)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans (MonadIO(..))

import Control.Applicative ((<$>))

cnv :: Double -> Int
-- NB (minBound :: Int64) == fromEnum (toEnum (maxBound :: Int64) :: Double)
--
-- NB How to best choose the divisor?
cnv d | d > 0     = fromEnum (    d * toEnum (maxBound `div` 16))
      | otherwise = fromEnum (abs d * toEnum (minBound `div` 16))

cnv32 :: Double -> Int32
-- NB How to best choose the divisor?
cnv32 d | d > 0     = toEnum $ fromEnum $
            d * (toEnum $ fromEnum (maxBound `div` 8 :: Int32))
      | otherwise = toEnum $ fromEnum $
        abs d * (toEnum $ fromEnum (minBound `div` 8 :: Int32))

cnv16 :: Double -> Int16
-- NB How to best choose the divisor?
cnv16 d | d > 0     = toEnum $ fromEnum $
            d * (toEnum $ fromEnum (maxBound `div` 8 :: Int16))
      | otherwise = toEnum $ fromEnum $
        abs d * (toEnum $ fromEnum (minBound `div` 8 :: Int16))

ecc_mutation :: (Functor m,PrimMonad m,MonadIO m) =>
  Int -> M Bool -> M Bool -> Noisyness -> Int -> m (ECC m V V Int16 Int)
ecc_mutation maxIterations h g noisyness untxBits =
  create >>= \gen -> return $ ECC
  { generate = listArray (1,originalBits) <$> generateList gen originalBits
  , encode = return . encoder g
    -- do not transmit untxBits parity bits
  , txRx = fmap (listArray (1,allBits))
         . fmap (map cnv16)
         . fmap (++ replicate untxBits 0)
         . addNoise gen rate noisyness
         . take frameSize
         . elems
  , decode = return . decoder_mutation maxIterations h
  , check = \x y -> checkList (elems x) (take originalBits $ elems y)
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

ecc_mutation_sparse :: (Functor m,PrimMonad m,MonadIO m) =>
  Int -> M Bool -> M Bool -> Noisyness -> Int -> m (ECC m V V Int16 Int)
ecc_mutation_sparse maxIterations h g noisyness untxBits =
  (\ecc -> ecc{decode = return . CRS.decoder_mutation maxIterations sparse_h}) <$>
  ecc_mutation maxIterations h g noisyness untxBits
  where !sparse_h = mkUCRS False h
