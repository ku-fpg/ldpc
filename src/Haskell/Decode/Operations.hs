{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}

module Haskell.Decode.Operations where

import Data.Int (Int32,Int16)

import qualified Prelude as P
import Prelude hiding (Num(..),Ord(..))

import Data.Monoid (Monoid(..))

type family Sign d :: *

class (Eq d,Monoid (Sign d)) => Operations d where
  signum :: d -> Sign d
  abs :: d -> d
  negate :: d -> d
  zero :: d
  (<) :: d -> d -> Bool
  threeFourths :: d -> d
  (+) :: d -> d -> d
  (-) :: d -> d -> d
  (*) :: Sign d -> d -> d

min a b = if a < b then a else b

type instance Sign Double = SignDouble
newtype SignDouble = SignDouble Double
instance Monoid SignDouble where
  mempty = SignDouble 1
  mappend (SignDouble x) (SignDouble y) = SignDouble (x P.* y)

instance Operations Double where
  signum d = SignDouble (P.signum d)
  abs = P.abs
  negate = P.negate
  zero = 0
  (<) = (P.<)
  threeFourths = ((3 / 4) P.*)
  (+) = (P.+)
  (-) = (P.-)
  SignDouble s * d = s P.* d

type instance Sign Int = SignInt
newtype SignInt = SignInt Int
instance Monoid SignInt where
  mempty = SignInt 1
  mappend (SignInt x) (SignInt y) = SignInt (x P.* y)

instance Operations Int where
  signum i = SignInt (P.signum i)
  -- surprisingly: P.abs minBound == minBound   !!
  abs    i = if i == minBound then maxBound else P.abs i
  -- surprisingly: P.negate minBound == minBound   !!
  negate i = if i == minBound then maxBound else P.negate i
  zero = 0
  (<) = (P.<)
  threeFourths i = let !half = i `div` 2
                   in half P.+ (half `div` 2)
  -- avoids overflows
  x + y = if x P.> 0 && y P.> 0 && z P.<= 0
          then maxBound
          else if x P.< 0 && y P.< 0 && z P.>= 0 then minBound
          else z
    where z = x P.+ y
  -- avoids overflows
  x - y = if x P.> 0 && y P.< 0 && z P.<= 0
          then maxBound
          else if x P.< 0 && y P.> 0 && z P.>= 0 then minBound
          else z
    where z = x P.- y
  SignInt s * d = s P.* d

type instance Sign Int32 = SignInt32
newtype SignInt32 = SignInt32 Int32
instance Monoid SignInt32 where
  mempty = SignInt32 1
  mappend (SignInt32 x) (SignInt32 y) = SignInt32 (x P.* y)

instance Operations Int32 where
  signum i = SignInt32 (P.signum i)
  -- surprisingly: P.abs minBound == minBound   !!
  abs    i = if i == minBound then maxBound else P.abs i
  -- surprisingly: P.negate minBound == minBound   !!
  negate i = if i == minBound then maxBound else P.negate i
  zero = 0
  (<) = (P.<)
  threeFourths i = let !half = i `div` 2
                   in half P.+ (half `div` 2)
  -- avoids overflows
  x + y = if x P.> 0 && y P.> 0 && z P.<= 0
          then maxBound
          else if x P.< 0 && y P.< 0 && z P.>= 0 then minBound
          else z
    where z = x P.+ y
  -- avoids overflows
  x - y = if x P.> 0 && y P.< 0 && z P.<= 0
          then maxBound
          else if x P.< 0 && y P.> 0 && z P.>= 0 then minBound
          else z
    where z = x P.- y
  SignInt32 s * d = s P.* d

type instance Sign Int16 = SignInt16
newtype SignInt16 = SignInt16 Int16
instance Monoid SignInt16 where
  mempty = SignInt16 1
  mappend (SignInt16 x) (SignInt16 y) = SignInt16 (x P.* y)

instance Operations Int16 where
  signum i = SignInt16 (P.signum i)
  -- surprisingly: P.abs minBound == minBound   !!
  abs    i = if i == minBound then maxBound else P.abs i
  -- surprisingly: P.negate minBound == minBound   !!
  negate i = if i == minBound then maxBound else P.negate i
  zero = 0
  (<) = (P.<)
  threeFourths i = let !half = i `div` 2
                   in half P.+ (half `div` 2)
  -- avoids overflows
  x + y = if x P.> 0 && y P.> 0 && z P.<= 0
          then maxBound
          else if x P.< 0 && y P.< 0 && z P.>= 0 then minBound
          else z
    where z = x P.+ y
  -- avoids overflows
  x - y = if x P.> 0 && y P.< 0 && z P.<= 0
          then maxBound
          else if x P.< 0 && y P.> 0 && z P.>= 0 then minBound
          else z
    where z = x P.- y
  SignInt16 s * d = s P.* d
