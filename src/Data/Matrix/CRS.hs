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

import Data.Array.Matrix (M, V)
import Data.Bit

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
    _bounds   :: {-# UNPACK #-} !((Int,Int), (Int,Int)),
    _values   :: !(arr Idx e)   ,    -- ^ A ; the non-zero values
    _col      :: !(UArray Idx Col),  -- ^ IJ; the column of each element in A
    _rowStart :: !(UArray Row Idx)   -- ^ IA; where each rows starts in A
    }

type Row = Int
type Col = Int
type Idx = Int

{-# NOINLINE mkUCRS #-}
mkUCRS :: (Eq e,IArray arr e) => e -> arr (Int,Int) e -> UCRS arr e
mkUCRS zero arr = UCRS zero bds  values col rowStart where
  bds@((rBase,cBase),(rTop,cTop)) = bounds arr
  numRows = rangeSize (rBase,rTop)
  numCols = rangeSize (cBase,cTop)

  nnz = length $ filter (/=zero) $ A.elems arr

  values = listArray (0,nnz-1) $ filter (/=zero) $ A.elems arr

  col = listArray (0,nnz-1) $ map (snd.fst) $
        filter ((/=zero).snd) $ A.assocs arr

  rowStart = listArray (rBase,rTop+1) $ go 0 rBase (A.assocs arr) where
    go !k !row !assocs
      | k == nnz = [nnz]
      | otherwise = case assocs of
        [] -> replicate (rTop-row+1) nnz -- the rest of the rows are empty
        ((thisRow,_),e):assocs'
          | e==zero   -> go k     row assocs' -- skip
          | thisRow<row     -> go (k+1) row assocs' -- count and skip
          | otherwise -> replicate (thisRow-row+1) k ++ go (k+1) (thisRow+1) assocs'

{-# INLINE unsafeAt #-}
unsafeAt :: (IArray arr e,Ord e) => UCRS arr e -> (Row,Col) -> e
unsafeAt ucrs (r,c) = binary_search (_col ucrs) rowStart rowStop c k (_zero ucrs)
  where
    k = A.unsafeAt (_values ucrs)
    rowStart = (!) (_rowStart ucrs) r
    rowStop  = (!) (_rowStart ucrs) (r+1) - 1
         -- decr b/c the array element is the index corresponding to the start
         -- of the next row

{-# INLINE binary_search #-}
binary_search :: (IArray UArray a,Ord a) => UArray Int a -> Int -> Int -> a -> (Int -> r) -> r -> r
-- start,stop: inclusive indices to search
binary_search !arr !start !stop target k kfail
  | start>stop = kfail
  | otherwise  = case compare target (A.unsafeAt arr mid) of
    LT -> binary_search arr start   (mid-1) target k kfail
    EQ -> k mid
    GT -> binary_search arr (mid+1) stop    target k kfail
  where mid = start + ((stop-start) `div` 2) -- avoids overflow



-- ==== row-major indexing

{-# INLINE foldlMRow #-}
foldlMRow :: (IArray arr e,Monad m) =>
  UCRS arr e -> acc -> (acc -> Row -> m acc) -> m acc
foldlMRow ucrs nil snoc =
  foldlMRow_esc ucrs nil (\acc r -> Right `liftM` snoc acc r)

{-# INLINE foldlMRow_esc #-}
foldlMRow_esc :: (IArray arr e,Monad m) =>
  UCRS arr e -> acc -> (acc -> Row -> m (Either acc acc)) -> m acc
foldlMRow_esc ucrs nil snoc = go rBase nil where
  ((rBase,_) ,(rTop, _)) = _bounds ucrs
  go !row !acc
    | row >= rTop = return acc
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
    --  FIXME EDK ??? what iis rowStop doing here ?
    ((!) rs row0) rowStop
    nil
    where rowStop = (!) rs (row0+1)

forMCol :: (IArray arr e,Monad m) => UCRS arr e -> Row -> ((Col,Idx) -> m ()) -> m ()
forMCol ucrs row snoc = foldlMCol ucrs row () (const snoc)

{-# INLINE foldlM #-}
foldlM :: (IArray arr e,Monad m) =>
  UCRS arr e -> acc -> (acc -> (Row,Col,Idx) -> m acc) -> m acc
foldlM ucrs nil snoc =
  aux_foldlM_esc ucrs (\acc rci -> Right `liftM` snoc acc rci)
    0 ((!) (_rowStart ucrs) (rBase + 1))
    cBase cTop
    nil
        where ((rBase, cBase), (rTop, cTop)) = _bounds ucrs

{-# INLINE aux_foldlM_esc #-}
aux_foldlM_esc :: (IArray arr e,Monad m) =>
  UCRS arr e -> (acc -> (Row,Col,Idx) -> m (Either acc acc)) ->
  Row -> Row -> Idx -> Idx ->
  acc -> m acc
aux_foldlM_esc !ucrs snoc = go where
  go !row !rowStop !i !stop !acc
   | i >= stop = return acc
   | i >= rowStop = go (row+1) ((!) (_rowStart ucrs) (row+2)) i stop acc
   | otherwise =
     (snoc acc (row,A.unsafeAt (_col ucrs) i,i) >>=) $ either return $
     go row rowStop (i+1) stop

--- derived operations

mapUCRS :: (IArray arr a,IArray arr b) =>
  (a -> b) -> UCRS arr a -> UCRS arr b
mapUCRS f ucrs = ucrs{_zero=f (_zero ucrs)
                     ,_values=A.amap f (_values ucrs)
                     }

assocsUCRS :: IArray a t => UCRS a t -> [((Row, Col), t)]
assocsUCRS UCRS {_values=values, _bounds=((rBase,_), (rTop,_)), _col=colindices, _rowStart=rs } =
    [ ((row, A.unsafeAt colindices j), A.unsafeAt values j)
                   | row <- [rBase .. rTop], j <- [ (!) rs row .. (!) rs (row + 1) - 1]]

indicesUCRS :: IArray a t => UCRS a t -> [(Row, Col)]
indicesUCRS ucrs = [index | (index,_) <- assocsUCRS ucrs]


ucrsToMatrix ::  (Eq a, Ord a, IArray arr a) =>  UCRS arr a -> M a
ucrsToMatrix ucrs  = accumArray (flip const) (_zero ucrs) (_bounds ucrs) (assocsUCRS ucrs)

--- for use with Worker/Wrapper

-- FIXME Question:  Should the mask argument to these funcitons really be M Bit ?
-- UCRS Bit would be more space and time efficient

-- FIXME  Is it an error if: mask ! (i,j) == Zero AND dense ! (i,j) /= zeroValue ???
toSparse :: (Eq a,IArray arr a) =>  M Bit -> M a ->  UCRS arr a
toSparse mask dense = UCRS zeroValue bds values col rowStart
    where   bds@((rBase,cBase),(rTop,cTop)) = bounds dense
            -- FIXME Question: If the mask is ONE everywhere, this will fail.
            -- Should this function take a 'zero' argument ?
            zeroValue = dense ! (head $ filter (\ i -> (mask ! i) ==  Zero) (A.indices mask))
            addresses = filter (\ i -> (mask ! i) /=  Zero) $ A.indices mask
            nnz = length $ addresses
            values = listArray (0,nnz-1) $ map (\i -> dense ! i) addresses
            col = listArray (0,nnz-1) $ map snd $ addresses
            rowStart = listArray  (rBase,rTop+1) $ go 0 rBase addresses where
                go !k !row !addrs
                     | k == nnz = [nnz]
                     | otherwise = case addrs of
                                    [] -> replicate (rTop-row+1) nnz -- the rest of the rows are empty
                                    ((thisRow,_)):addrs'
                                        | thisRow<row     -> go (k+1) row addrs' -- count and skip
                                        | otherwise -> replicate (thisRow-row+1) k ++ go (k+1) (thisRow+1) addrs'

-- FIXME Question.  Why does frSparse require/accept a mask matrix input ?
-- the ucrs already encodes a mask.
-- Is this argument intended to be a verification of equality of the masks?
-- If so, that is NOT presently done.
frSparse ::  (Eq a, Ord a, IArray arr a) =>  M Bit -> UCRS arr a -> M a
frSparse mask ucrs | (bounds mask) == (_bounds ucrs)  = dense
                   | otherwise = error "Mask and CRS matrix have different bounds"
    where addresses = filter (\ i -> (mask ! i) /=  Zero) $ A.indices mask
          dense = (fmap (const (_zero ucrs) ) mask ) //   map (\ i -> (i, unsafeAt ucrs i)) addresses

-- FIXME Question.  What purpose does the ucrs argument serve?
-- This function replaces every non-zero element of the sparse matrix -- without seeing the original
-- (assuming the mask represents exactly the mask appearing in ucrs).
-- I suppose that it could be available for "modification in place", but I don't expect that to happen soon,
-- since the current implementation of mkUCRS generates an output with internal immutable arrays
mapWithIndex :: (Eq a, Ord a, IArray arr a) => M Bit -> ((Row,Col) -> a) -> UCRS arr a -> UCRS arr a
mapWithIndex mask f sm = undefined -- sm // [(index, f index) | (index, v) <- A.assocs mask, v == One]


{- "Nick" forall mask eta f. toSparse (frSparse mask eta // [ (idx,f idx) | idx <- indices (frSparse mask eta), mask!idx == 1 ])
                                = SM.mapWithIndex mask f eta
-}
