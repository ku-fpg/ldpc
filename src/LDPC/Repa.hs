{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

-- there are all the Repa recommended options; I've commented out the ones that
-- get rejected in an OPTIONS_GHC pragma
{-# OPTIONS_GHC -Odph -fno-liberate-case #-}
--{-# OPTIONS_GHC -threaded -funfolding-use-threshold1000 -funfolding-keeness-factor1000
--{-# OPTIONS_GHC -fllvm -optlo-O3 #-}

module Haskell.Repa where

import qualified Haskell.Decode.Operations as O
import Data.Int (Int16)

import Data.Array.Repa as R
import Data.Array.Repa.Repr.Unboxed as R

import Debug.Trace (trace)

import Prelude

type VU = Array U DIM1
type MU = Array U DIM2

encoder :: MU Bool -> VU Bool -> VU Bool
-- vector-matrix multiply, lam * g
encoder g lam = R.foldS (/=) False $
                R.zipWith (&&) (transpose g) (transpose lams)
  where lams = extend (Any :. (nCols::Int)) lam
        Z :. _ :. nCols = extent g

parity :: MU Bool -> VU Bool -> Bool
-- matrix-vector multiply, h * lam, followed by a check for all zeros
parity h lam = not $ (! Z) $ foldS (||) False $ -- check for all zeros
               foldS (/=) False $ -- finish the dot product
               R.zipWith (&&) h $ -- start the dot product
               transpose lams
  where lams = extend (Any :. (nRows::Int)) lam
        Z :. nRows :. _ = extent h

decoder_Int16 :: Int -> MU Bool -> VU Int16 -> (Int,VU Bool)
decoder_Int16 maxIterations h lam0 = go 0 (computeS eta0,lam0) where
  eta0 = R.map (const O.zero) h

  go :: Int -> (MU Int16,VU Int16) -> (Int,VU Bool)
  go !n (!eta,!lam)
    | n >= maxIterations || parity h cHat
      = (n,cHat)
    | otherwise = -- trace ("iteration " Prelude.++ show n) $
      go (n+1) (step h lam0 eta lam)
    where cHat = computeS $ R.map (O.zero O.<) lam

step :: MU Bool -> VU Int16 -> MU Int16 -> VU Int16 -> (MU Int16,VU Int16)
step h lam0 eta lam = (computeS eta'',computeS lam') where
  etaSH@(Z :. nRows :. nCols) = extent eta

  -- replicate lam as a row
  lams = computeS $ extend (Any :. (nRows::Int) :. All) lam

  eta' = R.map (\(b,e,l) -> if b then e O.- l else O.zero) $ R.zip3 h eta lams

  -- each element becomes an array of its row-siblings
  explode :: Source r e => Array r DIM2 e -> Array D DIM3 e
  explode = backpermute (etaSH :. nSiblings) f where
    nSiblings = nCols-1

    f (Z :. row :. col :. col')
      | col' < col = Z :. row :. col' -- don't skip ourself yet
      | otherwise  = Z :. row :. (col'+1) -- now skip ourself

  eta'' = R.map (O.negate . O.threeFourths) $
          -- reduce the row-sibling dimension back to one scalar
          foldS O.min_dagger maxBound $
          -- replace each element with its row-siblings
          explode $
          -- sparsity: replace sparse matrix 'zero' by maxBound
          R.zipWith (\b e -> if b then e else maxBound) h $
          eta'

  lam' = R.zipWith (O.+) lam0 $
         foldS (O.+) O.zero . transpose $ -- add up the columns
         -- sparsity: replace sparse matrix 'zero' by maxBound
         R.zipWith (\b e -> if b then e else O.zero) h $
         eta''
