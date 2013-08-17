{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Haskell.Decode.Operations where

import qualified Prelude as P

import Data.Monoid (Monoid(..))

type family Sign d :: *

class (P.Eq d,Monoid (Sign d)) => Operations d where
  signum :: d -> Sign d
  abs :: d -> d
  negate :: d -> d
  zero :: d
  (<) :: d -> d -> P.Bool
  threeFourths :: d -> d
  (+) :: d -> d -> d
  (-) :: d -> d -> d
  (*) :: Sign d -> d -> d

min a b = if a < b then a else b

type instance Sign P.Double = SignDouble
newtype SignDouble = SignDouble P.Double
instance Monoid SignDouble where
  mempty = SignDouble 1
  mappend (SignDouble x) (SignDouble y) = SignDouble (x P.* y)

instance Operations P.Double where
  signum d = SignDouble (P.signum d)
  abs = P.abs
  negate = P.negate
  zero = 0
  (<) = (P.<)
  threeFourths = ((3 P./ 4) P.*)
  (+) = (P.+)
  (-) = (P.-)
  SignDouble s * d = s P.* d
