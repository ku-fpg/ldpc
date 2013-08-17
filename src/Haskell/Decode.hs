{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Haskell.Decode where

import Haskell.ArraySig

import Data.Array.Base (IArray(..),listArray,amap,elems,MArray)
import Data.Array.Base (thaw,unsafeFreeze)
import Data.Array.Unboxed ((!),Ix(..))
import Data.Array.ST (STUArray,readArray,writeArray,newArray)
import Control.Monad.ST (ST,runST)

import Control.Monad (when)
import Control.Applicative ((<$>))

import Data.Maybe (fromJust)

--import Control.Monad.ST.Unsafe (unsafeIOToST)

type STM s = STUArray s (Int,Int)
type STV s = STUArray s Int

updateArray :: (MArray a e m,Ix i) => a i e -> i -> (e -> m e) -> m ()
updateArray a i f = readArray a i >>= f >>= writeArray a i
{-# INLINE updateArray #-}

decoder_mutation :: Int -> M Bool -> V Double -> (Int,V Bool)
decoder_mutation maxIterations h lam0
  | len /= numCol = error "Haskell.Encode.decoder: bad dimensions"
  | otherwise = runST $ do
    lam <- thaw lam0
    eta <- thaw (amap (const 0) h)
    n <- go 0 lam eta
    -- unsafeFreeze is safe because lam dies
    (,) n . amap (>0) . trim <$> unsafeFreeze lam where

  !len = rangeSize lamBounds
  lamBounds@(lamBase,_) = bounds lam0
  !numCol = rangeSize (cBase,cTop)
  !numRow = rangeSize (rBase,rTop)
  ((rBase,cBase),(rTop,cTop)) = bounds h

  -- numRow is the number of extra parity bits, so drop that many from the
  -- result vector
  trim :: V Double -> V Double
  trim = listArray (1,numCol-numRow) . elems

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

  forEtaCol :: Monad m => Int -> (Int -> m ()) -> m ()
  forEtaCol row f = mapM_ (\col -> when (h!(row,col)) $ f col) $ range (cBase,cTop)
  {-# INLINE forEtaCol #-}

  allEtaRow :: Monad m => (Int -> m Bool) -> m Bool
  allEtaRow f = go (range (rBase,rTop)) where
    go !rows = case rows of
      [] -> return True
      (row:rows) -> f row >>= \b -> if b then go rows else return False

  foldlEtaCol :: Monad m => acc -> Int -> (acc -> Int -> m acc) -> m acc
  foldlEtaCol z row f = go (range (cBase,cTop)) z where
    go !cols !acc = case cols of
      [] -> return acc
      (col:cols) -> (if h!(row,col) then f acc col else return acc) >>= go cols

  forLamCol :: Monad m => (Int -> m ()) -> m ()
  forLamCol f = mapM_ f (range lamBounds)

  colEtaToLam :: Int -> Int
  colEtaToLam col = col-cBase+lamBase

{-  debug = unsafeIOToST -- switch to the bottom one to enable debugging
  debug :: IO () -> ST s ()
  debug = const (return ())

  rnd :: Double -> Double
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

  go :: Int -> STV s Double -> STM s Double -> ST s Int
  go !n !lam !eta
    | n >= maxIterations = return n
    | otherwise = do
    -- this is a short-circuiting "all"
    parity <- allEtaRow $ \row -> foldlEtaCol True row $ \parity col ->
      -- /= is xor; start with True so that the result is True if we have
      -- parity (ie an even number of ones)
      (/= parity) . (>0) <$> readArray lam (colEtaToLam col)

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
  go' :: Int -> STV s Double -> STM s Double -> ST s Int
  go' !n !lam !eta = do
--    unsafeIOToST $ putStr "iteration " >> print n

    -- eta[r,c] := eta[r,c] - lam[c]
    forEta $ \idx@(_,col) ->
      updateArray eta idx $ \e -> (e-) <$> readArray lam (colEtaToLam col)

    -- lam[c] := lam0[c]   -- is there a bulk operation for this?
    forLamCol $ \col -> writeArray lam col $ lam0!col

    -- eta[r,c] := min_dagger(eta[r,forall d. d /= c])
    -- lam[c]   := lam[c] + sum(eta[forall r. r,c])
    forEtaRow $ \row -> do
{-
      -- this is the clever way: take the whole row's min_dagger, then tweak it
      -- at each element

      -- collect the minimum and the next-most minimum in the whole row
      -- NB assumes each row of eta & h has at least two ones
      (minSign,TwoMD the_min the_2nd_min) <- do
        let snoc (!sign,!md) !x = (sign * signum x,minMD md $ abs x)
        foldlEtaCol (1,ZeroMD) row $ \mins col -> snoc mins <$> readArray eta (row,col)

      forEtaCol row $ \col -> do
        etav <- readArray eta (row,col)
        -- sinblingMin is the min_dagger of this element's same-row
        -- siblings. We recover it from the whole row's TwoMD value by
        -- "subtracting" this element.
        let siblingMin = minSign * signum etav *
              if abs etav == the_min -- dubious use of (==) Double
              then the_2nd_min else the_min
            etav' = negate $ 0.75*siblingMin
        writeArray eta (row,col) etav'
-}

      -- this, on the other hand, is the straight-forward way. (We might
      -- optimize to add the_mins array as a loop argument)

      the_mins <- newArray (cBase,cTop) 0
        -- stash the minimums here as we compute them, since we need to retain
        -- the previous value for the minimum computations

      forEtaCol row $ \col -> do
        x <- foldlEtaCol Nothing row $ \the_min col2 ->
          if col == col2 then return the_min else do
            !etav <- readArray eta (row,col2)
            return $ Just $ maybe etav (min_dagger etav) the_min
        -- NB the fromJust is safe unless there is an empty row in h
        writeArray (the_mins `asTypeOf` lam) col $ fromJust x


      forEtaCol row $ \col -> do
        the_min <- readArray the_mins col
        let etav' = negate $ 0.75 * the_min
        writeArray eta (row,col) etav'

        -- add the new eta value to lam
        updateArray lam (colEtaToLam col) $ return . (+etav')

    go (n+1) lam eta

--  showV = putStrLn . map (\b -> if b then '1' else '0') . elems

{-# INLINE min_dagger #-}
min_dagger x y = signum x * signum y * min (abs x) (abs y)

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
