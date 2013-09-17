{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Haskell.Decode where

import qualified Haskell.Decode.Operations as O
import Data.Int (Int16)

import Haskell.ArraySig

import Data.Array.Base (IArray(..),amap,MArray)
import Data.Array.Base (thaw,unsafeFreeze)
import Data.Array.Unboxed ((!),Ix(..))
import Data.Array.ST (STUArray,readArray,writeArray,newArray)
import Control.Monad.ST (ST,runST)

import Data.Maybe (fromJust)

import Control.Monad (when)
import Control.Applicative ((<$>))

--import Control.Monad.ST.Unsafe (unsafeIOToST)

type STM s = STUArray s (Int,Int)
type STV s = STUArray s Int

updateArray :: (MArray a e m,Ix i) => a i e -> i -> (e -> m e) -> m ()
updateArray a i f = do
  !v <- readArray a i
  f v >>= writeArray a i
{-# INLINE updateArray #-}

type D = Int16 -- NB we use == D, so Double might be questionable

decoder_mutation :: Int -> M Bool -> V D -> (Int,V Bool)
decoder_mutation
  maxIterations h lam0
  | len /= numCol = error "Haskell.Decode.decoder: bad dimensions"
  | otherwise = runST $ do
    lam <- thaw lam0
    eta <- thaw (amap (const O.zero) h)
    n <- go 0 lam eta
    -- unsafeFreeze is safe because lam dies
    (,) n . amap isPositive <$> unsafeFreeze lam where

  !len = rangeSize lamBounds
  lamBounds@(lamBase,_) = bounds lam0
  !numCol = rangeSize (cBase,cTop)
  ((rBase,cBase),(rTop,cTop)) = bounds h

  isPositive = (O.zero O.<) :: D -> Bool
  isNegative = (O.< O.zero) :: D -> Bool

  forEta :: Monad m => ((Int,Int) -> m ()) -> m ()
--  forEta f = mapM_ (\idx -> when (h!idx) $ f idx) $ range (bounds h)
  -- the range hBounds floats out before it can fuse with mapM_

  forEta f = go rBase cBase where
    go !row !col
      | col>cTop = go (row+1) cBase
      | row>rTop = return ()
      | otherwise = (when (h!(row,col)) $ f (row,col)) >> go row (col+1)

  forEtaRow :: Monad m => (Int -> m ()) -> m ()
  forEtaRow f = mapM_ f $ range (rBase,rTop)

--  foldlEtaRow :: Monad m => acc -> (acc -> Int -> m acc) -> m acc
--  foldlEtaRow z f = go (range (rBase,rTop)) z where
--    go !rows !acc = case rows of
--      [] -> return acc
--      (row:rows) -> row `seq` f acc row >>= go rows

  forEtaCol :: Monad m => Int -> (Int -> m ()) -> m ()
  forEtaCol row f = mapM_ (\col -> when (h!(row,col)) $ f col) $ range (cBase,cTop)
  {-# INLINE forEtaCol #-}

  allEtaRow :: Monad m => (Int -> m Bool) -> m Bool
  allEtaRow f = go (range (rBase,rTop)) where
    go !rows = case rows of
      [] -> return True
      (row:rows) -> row `seq` f row >>= \b -> if b then go rows else return False

  foldlEtaCol :: Monad m => acc -> Int -> (acc -> Int -> m acc) -> m acc
  foldlEtaCol z row f = go (range (cBase,cTop)) z where
    go !cols !acc = case cols of
      [] -> return acc
      (col:cols) -> col `seq` (if h!(row,col) then f acc col else return acc) >>= go cols

  forLamCol :: Monad m => (Int -> m ()) -> m ()
  forLamCol f = mapM_ f (range lamBounds)

  colEtaToLam :: Int -> Int
  colEtaToLam col = col-cBase+lamBase

{-  debug = unsafeIOToST -- switch to the bottom one to enable debugging
  debug :: IO () -> ST s ()
  debug = const (return ())

  rnd :: d -> d
  rnd d = fromIntegral (round (d * 1000.0) :: Int) / 1000.0
  putStr7 "" = putStr "       "
  putStr7 s = pad $ let (pre,post) = break (=='.') s in
    pre ++ if null post then "    " else take 4 post
    where
      pad t = putStr $ if len < 7 then replicate (7 - len) ' ' ++ t else t
        where len = length t

  forEtaCol' :: Monad m => Int -> (Int -> Bool -> m ()) -> m ()
  forEtaCol' row f = mapM_ (\col -> f col (h!(row,col))) $ range (cBase,cTop)

  dumpEta eta s row = (debug (putStr s >> putStr " ") >>) $ (>> debug (putStrLn "")) $
    forEtaCol' row $ \col enabled ->
      if not enabled then debug $ putStr7 "" >> putStr " "
      else readArray eta (row,col) >>= \x -> debug $ putStr7 (show (rnd x)) >> putStr " "
-}

--  transpose_h = transpose h -- for use with the multVM implementation of parity

  go :: Int -> STV s D -> STM s D -> ST s Int
  go !n !lam !eta
    | n >= maxIterations = return n
    | otherwise = do
    -- NB we cannot straight-forwardly interleave the parity check with the
    -- decode iteration because the check depends on the resulting lam value
    -- that only exists after the whole iteration is complete

    -- this is a short-circuiting "all"
    parity <- allEtaRow $ \row -> foldlEtaCol True row $ \parity col ->
      -- /= is xor; start with True so that the result is True if we have
      -- parity (ie an even number of ones)
      (/= parity) . isPositive <$> readArray lam (colEtaToLam col)

{- -- this would be a more convenient way of expressing the parity check, but
   -- it allocates a lot and does not short-circuit

    -- unsafeFreeze is safe because lam' doesn't survive this iteration
    parity <- unsafeFreeze lam >>= \lam' -> do
      let cHat = amap (>0) lam'
      return $ not $ or $ -- no ones
        elems $ multVM "decode" cHat transpose_h
-}
    if parity then return n else go' n lam eta

  {-# INLINE go' #-} -- we want a directly recursive go
  go' :: Int -> STV s D -> STM s D -> ST s Int
  go' !n !lam !eta = do
--    unsafeIOToST $ putStr "iteration " >> print n

    -- eta[r,c] := eta[r,c] - lam[c]
    forEta $ \idx@(_,col) ->
      updateArray eta idx $ \e -> (e O.-) <$> readArray lam (colEtaToLam col)

    -- lam[c] := lam0[c]   -- is there a bulk operation for this?
    forLamCol $ \col -> writeArray lam col $ lam0!col

    -- eta[r,c] := min_dagger(eta[r,forall d. d /= c])
    -- lam[c]   := lam[c] + sum(eta[forall r. r,c])
    forEtaRow $ clever lam eta

    go (n+1) lam eta

  -- NB @clever@ should be equivalent to @simple@

  {-# INLINE clever #-}
  clever :: STV s D -> STM s D -> Int -> ST s ()
  clever !lam !eta !row = do
    -- this is the clever way: take the whole row's min_dagger, then tweak it
    -- at each element

    -- collect the minimum and the next-most minimum in the whole row
    -- NB assumes each row of eta & h has at least two ones
    (minSign,O.TwoMD the_min the_2nd_min) <- do
        let snoc (!sign,!md) !x = (sign /= isNegative x,O.minMD md $ O.abs x)
        foldlEtaCol (False {-is not negative-},O.ZeroMD) row $ \mins col ->
          snoc mins <$> readArray eta (row,col)

    forEtaCol row $ \col -> do
      etav <- readArray eta (row,col)
      -- siblingMin is the min_dagger of this element's same-row siblings. We
      -- recover it from the whole row's TwoMD value by "removing" this
      -- element from the row's minimum.
      let etaSign = isNegative etav
          siblingSign = minSign /= etaSign
          siblingAbs = if O.abs etav == the_min
                       then the_2nd_min else the_min
          siblingMin = if siblingSign then O.negate siblingAbs else siblingAbs
          etav' = O.negate $ O.threeFourths siblingMin

      writeArray eta (row,col) etav'

      -- add the new eta value to lam
      updateArray lam (colEtaToLam col) $ return . (O.+ etav')

  {-# INLINE _simple #-}
  _simple :: STV s D -> STM s D -> Int -> ST s ()
  _simple !lam !eta !row = do
    -- this, on the other hand, is the straight-forward way. (We could
    -- optimize by adding the_mins array as a loop argument.)

    the_mins <- newArray (cBase,cTop) O.zero
      -- stash the minimums here as we compute them, since we need to retain
      -- the previous value for the minimum computations

    forEtaCol row $ \col -> do
      -- intead of the Maybe, we could use maxBound, but then D could never be
      -- Double
      x <- foldlEtaCol Nothing row $ \the_min col2 ->
        if col == col2 then return the_min else do
          !etav <- readArray eta (row,col2)
          return $ Just $ maybe etav (O.min_dagger etav) the_min
      -- NB the fromJust is safe unless there is an empty row in h
      writeArray (the_mins `asTypeOf` lam) col $ fromJust x

    forEtaCol row $ \col -> do
      the_min <- readArray the_mins col
      let etav' = O.negate $ O.threeFourths the_min
      writeArray eta (row,col) etav'

      -- add the new eta value to lam
      updateArray lam (colEtaToLam col) $ return . (O.+ etav')
