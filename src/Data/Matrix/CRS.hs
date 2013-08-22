{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

-- We don't want GHC to create sharing among the indices in mkUCRS, assuming
-- that the original matrix is pretty large. Sharing would create huge lists.
--
-- This option doesn't really effect compilation of the other functions in this
-- module because they are all INLINE.
{-# OPTIONS_GHC -fno-cse #-}

module Data.Matrix.CRS where

-- CRS is Compressed Row Storage, aka Compressed Sparse Row. These names are
-- widely used.
--
-- Main idea: store the non-zero values in row-major order and track the
-- original coordinates using additional arrays.

import qualified Data.Array.Base as A
import Data.Array.Unboxed
import Control.Monad (liftM)

-- | Unboxed CRS representation of a numRows-by-numCols matrix M.
--
-- NNZ is the number of non-zero elements in M.
--
--   |values| == NNZ
--   |col| == NNZ
--   |rowStart| == numRows+1
--
-- The values from rowStart[r] to rowStart[r+1]-1 have a row-index of r.
data UCRS arr e
  = UCRS {
    _zero     :: !e,
    _numCol   :: {-# UNPACK #-} !Int,
    _values   :: !(arr Idx e)   ,    -- ^ A ; the non-zero values
    _col      :: !(UArray Idx Col),  -- ^ IJ; the column of each element in A
    _rowStart :: !(UArray Row Idx)   -- ^ IA; where each rows starts in A
    }

extentCRS :: UCRS arr e -> (Int,Int)
extentCRS ucrs = (A.numElements (_rowStart ucrs) - 1,_numCol ucrs)

type Row = Int
type Col = Int
type Idx = Int

{-# NOINLINE mkUCRS #-}
mkUCRS :: (Eq e,IArray arr e) => e -> arr (Int,Int) e -> UCRS arr e
mkUCRS zero arr = UCRS zero numCols values col rowStart where
  ((rBase,cBase),(rTop,cTop)) = bounds arr
  numRows = rangeSize (rBase,rTop)
  numCols = rangeSize (cBase,cTop)

  nnz = length $ filter (/=zero) $ A.elems arr

  values = listArray (0,nnz-1) $ filter (/=zero) $ A.elems arr

  col = listArray (0,nnz-1) $ map (subtract cBase.snd.fst) $
        filter ((/=zero).snd) $ A.assocs arr

  rowStart = listArray (0,numRows) $ go 0 0 (A.assocs arr) where
    go !k !row !assocs
      | k == nnz = [nnz]
      | otherwise = case assocs of
        [] -> replicate (numRows-row+1) nnz -- the rest of the rows are empty
        ((rRaw,_),e):assocs'
          | e==zero   -> go k     row assocs' -- skip
          | r<row     -> go (k+1) row assocs' -- count and skip
          | otherwise -> replicate (r-row+1) k ++ go (k+1) (r+1) assocs'
          where r = rRaw-rBase

{-# INLINE unsafeAt #-}
unsafeAt :: (IArray arr e,Ord e) => UCRS arr e -> (Row,Col) -> e
unsafeAt ucrs (r,c) = binary_search (_col ucrs) rowStart rowStop c k (_zero ucrs)
  where
    k = A.unsafeAt (_values ucrs)
    rowStart = A.unsafeAt (_rowStart ucrs) r
    rowStop  = A.unsafeAt (_rowStart ucrs) (r+1)

{-# INLINE binary_search #-}
binary_search :: (IArray UArray a,Ord a) => UArray Int a -> Int -> Int -> a -> (Int -> r) -> r -> r
binary_search !arr !start !stop target k kfail
  | start+1 == stop = if e == target then k mid else kfail
  | otherwise       = case compare e target of
    LT -> binary_search arr start   mid  target k kfail
    EQ -> k mid
    GT -> binary_search arr (mid+1) stop target k kfail
  where e = A.unsafeAt arr mid
        mid = (start+stop) `div` 2



-- ==== row-major indexing

{-# INLINE foldlMRow #-}
foldlMRow :: (IArray arr e,Monad m) =>
  UCRS arr e -> acc -> (acc -> Row -> m acc) -> m acc
foldlMRow ucrs nil snoc =
  foldlMRow_esc ucrs nil (\acc r -> Right `liftM` snoc acc r)

{-# INLINE foldlMRow_esc #-}
foldlMRow_esc :: (IArray arr e,Monad m) =>
  UCRS arr e -> acc -> (acc -> Row -> m (Either acc acc)) -> m acc
foldlMRow_esc ucrs nil snoc = go 0 nil where
  (stop,_) = extentCRS ucrs
  go !row !acc
    | row >= stop = return acc
    | otherwise = snoc acc row >>= either return (go (row+1))

forMRow :: (IArray arr e,Monad m) => UCRS arr e -> (Row -> m ()) -> m ()
forMRow ucrs snoc = foldlMRow ucrs () (const snoc)

{-# INLINE foldlMCol #-}
foldlMCol :: (IArray arr e,Monad m) =>
  UCRS arr e -> Row -> acc -> (acc -> (Col,Idx) -> m acc) -> m acc
foldlMCol ucrs row0 nil snoc =
  foldlMCol_esc ucrs row0 nil (\acc (c,i) -> Right `liftM` snoc acc (c,i))

{-# INLINE foldlMCol_esc #-}
foldlMCol_esc :: (IArray arr e,Monad m) =>
  UCRS arr e -> Row -> acc -> (acc -> (Col,Idx) -> m (Either acc acc)) -> m acc
foldlMCol_esc ucrs@UCRS{_rowStart=rs} row0 nil snoc =
  aux_foldlM_esc ucrs (\acc (_,c,i) -> snoc acc (c,i))
    row0 rowStop
    (A.unsafeAt rs row0) rowStop
    nil
    where rowStop = A.unsafeAt rs (row0+1)

forMCol :: (IArray arr e,Monad m) => UCRS arr e -> Row -> ((Col,Idx) -> m ()) -> m ()
forMCol ucrs row snoc = foldlMCol ucrs row () (const snoc)

{-# INLINE foldlM #-}
foldlM :: (IArray arr e,Monad m) =>
  UCRS arr e -> acc -> (acc -> (Row,Col,Idx) -> m acc) -> m acc
foldlM ucrs nil snoc =
  aux_foldlM_esc ucrs (\acc rci -> Right `liftM` snoc acc rci)
    0 (A.unsafeAt (_rowStart ucrs) 1)
    0 (A.numElements (_col ucrs))
    nil

{-# INLINE aux_foldlM_esc #-}
aux_foldlM_esc :: (IArray arr e,Monad m) =>
  UCRS arr e -> (acc -> (Row,Col,Idx) -> m (Either acc acc)) ->
  Row -> Row -> Idx -> Idx ->
  acc -> m acc
aux_foldlM_esc !ucrs snoc = go where
  go !row !rowStop !i !stop !acc
   | i >= stop = return acc
   | i >= rowStop = go (row+1) (A.unsafeAt (_rowStart ucrs) (row+2)) i stop acc
   | otherwise =
     (snoc acc (row,A.unsafeAt (_col ucrs) i,i) >>=) $ either return $
     go row rowStop (i+1) stop

--- derived operations

mapUCRS :: (IArray arr a,IArray arr b) =>
  (a -> b) -> UCRS arr a -> UCRS arr b
mapUCRS f ucrs = ucrs{_zero=f (_zero ucrs)
                     ,_values=A.amap f (_values ucrs)
                     }
