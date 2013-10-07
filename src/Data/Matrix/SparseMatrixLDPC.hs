{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-cse #-}

module Data.Matrix.SparseMatrixLDPC where

import Data.Array.Matrix (M, V, vector)
import Data.Bit

import qualified Data.Array.Base as A
import Data.Array.Unboxed
import Data.List(elemIndex)

-- A very specific representation for a Sparse Matrix useful in an LDPC decoder (SML)
-- (M Bit) is the mask, defining the size of the matrix and those indices that are non-zero
-- (V e) values at the mask'ed indices.
-- final 'e' is value for all non-masked indices
type SML e = (M Bit, V e, e)

type Row = Int
type Col = Int
type Idx = Int

bounds :: SML e -> ((Int,Int), (Int, Int))
bounds (mask, _, _) = A.bounds mask

maskIndices :: M Bit -> [(Int,Int)]
maskIndices mask = [ i | i <- A.indices mask, mask ! i == One]

{-# NOINLINE mkSML #-}
mkSML :: M Bit -> e -> e -> SML e
mkSML mask initVal zeroVal = (mask, values, zeroVal) where
  values = vector $ take maskLeng $ repeat initVal
  maskLeng = length $ maskIndices mask

{-# INLINE unsafeAt #-}
unsafeAt :: SML e -> (Row,Col) -> e
unsafeAt (mask , values, zeroVal) index
    | mask ! index == Zero = zeroVal
    | otherwise            = values ! vindex
  where
    Just vindex  = elemIndex index $ maskIndices mask

--- derived operations

-- WARNING:  Applies f to the zeroVal  (required for type signature)
mapSML :: (a -> b) -> SML a -> SML b
mapSML f (mask, values, zeroVal)  = (mask, A.amap f values, f zeroVal)

assocsSML :: SML e -> [((Row, Col), e)]
assocsSML (mask, values, _) = zip (maskIndices mask) (A.elems values)

indicesSML :: SML e -> [(Row, Col)]
indicesSML (mask, _, _) = maskIndices mask

--- for use with Worker/Wrapper

-- we rely on these two assumptions  (in Data.Array.Matrix)
-- type V a = Array Int a          -- We assume that we index (1,length array)
-- type M a = Array (Int,Int) a    -- We assume that we index ((1,1),(length A,length B))

-- FIXME  Is it an error if: mask ! (i,j) == Zero AND dense ! (i,j) /= zeroValue ???
toSparse :: (Eq e) =>  M Bit -> e -> M e -> SML e
toSparse mask zeroVal dense = (mask, values, zeroVal)
    where values= vector [ dense ! index | index <- maskIndices mask ]

frSparse ::  SML e -> M e
frSparse sml@(mask, values, zeroVal)  = dense
    where dense = (fmap (const zeroVal) mask ) //  (assocsSML sml)

mapWithIndex :: ((Row,Col) -> e) -> SML e -> SML e
mapWithIndex f sml@(mask, _, zeroVal)  = (mask, values, zeroVal)
    where values = vector $ map f (indicesSML sml)

{- "Nick" forall mask eta f. toSparse (frSparse mask eta // [ (idx,f idx) | idx <- indices (frSparse mask eta), mask!idx == 1 ])
                                = SM.mapWithIndex mask f eta
-}
