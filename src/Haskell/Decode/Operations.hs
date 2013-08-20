{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}

module Haskell.Decode.Operations where

import Data.Int (Int32,Int16)

import qualified Prelude as P
import Prelude hiding (Num(..),Ord(..))

import Data.Monoid (Monoid(..),(<>))

type family Signum d :: *

class (Eq d,Monoid (Signum d)) => Operations d where
  signum :: d -> Signum d
  abs :: d -> d
  negate :: d -> d
  zero :: d
  (<) :: d -> d -> Bool
  threeFourths :: d -> d
  (+) :: d -> d -> d
  (-) :: d -> d -> d
  (*) :: Signum d -> d -> d

min a b = if a < b then a else b

type instance Signum Double = SignumDouble
newtype SignumDouble = SignumDouble Double
instance Monoid SignumDouble where
  mempty = SignumDouble 1
  mappend (SignumDouble x) (SignumDouble y) = SignumDouble (x P.* y)

instance Operations Double where
  signum d = SignumDouble (P.signum d)
  abs = P.abs
  negate = P.negate
  zero = 0
  (<) = (P.<)
  threeFourths = ((3 / 4) P.*)
  (+) = (P.+)
  (-) = (P.-)
  SignumDouble s * d = s P.* d

type instance Signum Int = SignumInt
newtype SignumInt = SignumInt Int
instance Monoid SignumInt where
  mempty = SignumInt 1
  mappend (SignumInt x) (SignumInt y) = SignumInt (x P.* y)

instance Operations Int where
  signum i = SignumInt (P.signum i)
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
  SignumInt s * d = s P.* d

type instance Signum Int32 = SignumInt32
newtype SignumInt32 = SignumInt32 Int32
instance Monoid SignumInt32 where
  mempty = SignumInt32 1
  mappend (SignumInt32 x) (SignumInt32 y) = SignumInt32 (x P.* y)

instance Operations Int32 where
  signum i = SignumInt32 (P.signum i)
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
  SignumInt32 s * d = s P.* d

type instance Signum Int16 = SignumInt16
newtype SignumInt16 = SignumInt16 Int16
instance Monoid SignumInt16 where
  mempty = SignumInt16 1
  mappend (SignumInt16 x) (SignumInt16 y) = SignumInt16 (x P.* y)

instance Operations Int16 where
  signum i = SignumInt16 (P.signum i)
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
  SignumInt16 s * d = s P.* d



{-# INLINE min_dagger #-}
min_dagger :: Operations d => d -> d -> d
min_dagger x y = (signum x <> signum y) * min (abs x) (abs y)


-- INVARIANT all values in this data-structure are non-negative
data MD a = ZeroMD | OneMD a | TwoMD a a

-- INVARIANT all arguments non-negative
minMD ZeroMD x = OneMD x
minMD (OneMD a) x
  | x < a = TwoMD x a
  | otherwise = TwoMD a x
minMD (TwoMD a b) x
  | x < a = TwoMD x a
  | x < b = TwoMD a x
  | otherwise = TwoMD a b
{-# INLINE minMD #-}
