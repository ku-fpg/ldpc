{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module LDPC.Array.Decode2D where

import Data.Array.Matrix
import Data.Bit
import Data.Array
import ECC

import Control.Monad (liftM2,liftM4)

-- ==================================================
--             UNTESTED
--
-- but it compiles so smooth

-----------
-- the explorations in this module focus only on ne' and lam' for now

ldpc_skeleton :: forall d. (Num d,Ord d) =>
  (M Bit -> V d -> M d -> V d -> (M d,V d)) ->
  Int -> M Bit -> V d -> V Bit
ldpc_skeleton step maxIterations a orig_lam =
  loop 0 (fmap (const 0) a) orig_lam where
  loop :: Int -> M d -> V d -> V Bit
  loop !n !ne !lam
    | n >= maxIterations = fmap hard orig_lam
    | all (==0) $ elems $ a `mm` columnM c_hat = c_hat
    | otherwise = let (ne',lam') = step a orig_lam ne lam in loop (n+1) ne' lam'
    where c_hat = fmap hard lam

----------
-- spec using a generic matrix interface

step_idiomatic :: forall d. (Floating d, Ord d) =>
  M Bit -> V d -> M d -> V d -> (M d,V d)
step_idiomatic a orig_lam ne lam = (ne',lam') where
  ne' = fmap (\x -> -2 * atanh x)
      $ onEachRow_muscanlr (*) 1 (\lr _ rl -> lr*rl) a
      $ onEachRow_zipWith (\lamV neV -> tanh ((neV-lamV)/2)) lam a ne

  lam' = foldCols (+) orig_lam a ne'

----------
-- spec using a 2D decomposition (anticipating hardware)

step_2D :: forall d. (Floating d, Ord d) =>
  M Bit -> V d -> M d -> V d -> (M d,V d)
step_2D a orig_lam ne lam =
  decomp_vert True {- ie has the top row -} a orig_lam ne lam

above_ (neT,lamT) (neB,lamB) = (above neT neB,zipVWith (+) lamT lamB)

decomp_vert :: forall d. (Floating d, Ord d) =>
  Bool -> M Bit -> V d -> M d -> V d -> (M d,V d)
decomp_vert hasTopRow a orig_lam ne lam
  | Just ((aTop,aBot),(neTop,neBot)) <- liftM2 (,) (unabove a) (unabove ne) =
      decomp_vert hasTopRow aTop orig_lam neTop lam
        `above_`
      decomp_vert False aBot orig_lam neBot lam
  | otherwise = fst $ decomp_horiz hasTopRow a orig_lam ne lam (1,1)

-- Because ne' involves a mapping scan, we must pass along the partial
-- reduction. Because the scan is bidirectional, we need distinct inputs (ie
-- inherited attributes, ie downward-flowing information), one for
-- left-to-right and one for right-to-left.
type PartialIn d = (d,d)

-- In the case of a uniform (and hence associative) bidirectional scan, both
-- reductions require the same output (ie synthesized attribute, ie
-- upward-flowing information) from a given subtree.
type PartialOut d = d

-- Thus, for a uniform bidirectional scan, we need not *thread* the
-- left-to-right and right-to-left accumulators through all of the leaves. This
-- costs extra multiplies, so *does it materially improve the whole LDPC
-- circuit?*
beside_ left rite (fromL,fromR) =
  let -- NB knot is safe; prodL and prodR do not depend on the inputs to left
      -- and rite
      ((neL,lamL),prodL) = left (fromL,halfR)
      halfL = fromL*prodL
      ((neR,lamR),prodR) = rite (halfL,fromR)
      halfR = prodR*fromR
  in (,) (beside neL neR,appendV lamL lamR)
         (halfL*halfR)

-- `simpler_beside_` would require @PartialOut d = (d,d)@, @out =
-- (fromL*v,v*fromR)@ in `decomp_horiz`, and @zeroOut = partials@ in
-- `decomp_horiz`.
simpler_beside_ left rite (fromL,fromR) =
  let -- NB knot
      ((neL,lamL),(outL,inner_outR)) = left (fromL,inner_outR)
      ((neR,lamR),(inner_outL,outR)) = rite (inner_outL,fromR)
  in (,) (beside neL neR,appendV lamL lamR)
         (outL,outR)

decomp_horiz :: forall d. (Floating d, Ord d) =>
  Bool -> M Bit -> V d -> M d -> V d -> PartialIn d -> (,) (M d,V d) (PartialOut d)
decomp_horiz isTopRow a orig_lam ne lam partials@(fromL,fromR)
  | Just ((aLeft,aRite),(orig_lamLeft,orig_lamRite)
         ,(neLeft,neRite),(lamLeft,lamRite)
         ) <- liftM4 (,,,) (unbeside a) (unbesideV orig_lam)
                           (unbeside ne) (unbesideV lam) =
      ($ partials) $
      decomp_horiz isTopRow aLeft orig_lamLeft neLeft lamLeft
        `beside_`
      decomp_horiz isTopRow aRite orig_lamRite neRite lamRite
  | [[One]] <- fromMatrix a,
    [orig_lam1] <- fromVector orig_lam =
    let [[ne1]] = fromMatrix ne
        [lam1] = fromVector lam
        v = tanh ((ne1-lam1)/2)
        ne1' = -2 * atanh (fromL*fromR) -- product excludes v (ie this element)
        lam1' = if isTopRow then orig_lam1 + ne1' else ne1'
        out = v
    in (,) (matrix [[ne1']],vector [lam1']) out
  | [[Zero]] <- fromMatrix a =
    let zeroOut = 1
    in (,) (matrix [[0xdeadbeef]],vector [0]) zeroOut
  | otherwise = error "step_2D"
