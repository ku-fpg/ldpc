module Data.Bit where

import System.Random.MWC
import Control.Monad

data Bit = Zero | One
        deriving (Eq, Ord)

type Word1 = Bit

setBit :: Bool -> Bit
setBit False = Zero
setBit True  = One

getBit :: Bit -> Bool
getBit Zero = False
getBit One  = True

instance Show Bit where
    show Zero = "0"
    show One  = "1"

instance Read Bit where
    readsPrec p xs = case readsPrec p xs of
                       [ (0,ys) ] -> [ (Zero,ys) ]
                       [ (1,ys) ] -> [ (One,ys) ]
                       _ -> []

instance Num Bit where
    a + b = setBit $ a /= b        -- XOR
    One * One  = One            -- OR
    _   * _    = Zero
    a   - Zero = a
    Zero - One = One
    One  - One = Zero
    negate a   = a
    abs    a   = a
    signum a   = a

    fromInteger 0 = Zero
    fromInteger 1 = One
    fromInteger n = error $ show n ++ " :: Bit is not 0 or 1"

instance Variate Bit where
    uniform  = liftM setBit . uniform
    uniformR (a,b) = liftM setBit . uniformR (getBit a,getBit b)
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}
