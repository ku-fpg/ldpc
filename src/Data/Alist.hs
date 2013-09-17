-- This is the format defined in
-- http://www.inference.phy.cam.ac.uk/mackay/codes/alist.html
{-# LANGUAGE DataKinds, GADTs #-}
module Data.Alist where

import Data.Array
import Data.Bit
import Data.Bits
import Haskell.Array.Sig
import Control.Monad.State.Lazy
import Debug.Trace

import System.Random

class Alist a where
  readAlist  :: String -> IO a
  writeAlist :: String -> a -> IO ()



{-
data Alist = Alist
        { nm        :: (Int,Int)
        , num_nlist :: [Int]
        , num_mlist :: [Int]
        , nlist     :: Array Int [Int]
        , mlist     :: Array Int [Int]
        } deriving (Eq, Show)



readAlist :: String -> IO Alist
readAlist fileName = do txt <- readFile fileName
                        return $ evalState parser $ filter (/= 0) $ map read $ words txt
  where
    parser = do
        n <- item
        m <- item
        _ <- item
        _ <- item
        num_nlist <- replicateM n item
        num_mlist <- replicateM m item
        nlists <- sequence [ replicateM c item | c <- num_nlist ]
        mlists <- sequence [ replicateM c item | c <- num_mlist ]
        return $ Alist
               { nm = (n,m)
               , num_nlist = num_nlist
               , num_mlist = num_mlist
               , nlist = array (1,n) [ (i,x) | (i,x) <- [1..] `zip` take n nlists ]
               , mlist = array (1,m) [ (i,x) | (i,x) <- [1..] `zip` take m mlists ]
               }

writeAlist :: String -> Alist -> IO ()
writeAlist fileName alist = writeFile fileName $ unlines $ map unwords $ map (map show) $
        [ [ fst $ nm alist, snd $ nm alist ]
        , [ maximum $ num_nlist alist, maximum $ num_mlist alist ]
        , num_nlist alist
        , num_mlist alist
        ] ++ elems (nlist alist)
          ++ elems (mlist alist)

-- check for sanity
alistCheck :: Alist -> ()
alistCheck alist = ()

alistToArray :: Alist -> Array (Int,Int) Bit
alistToArray alist = accumArray (\ _ _ -> 1) 0 ((1,1),(fst $ nm alist, snd $ nm alist))
        [  ((n,m),())
        | (n,ms) <- assocs (nlist alist)
        , m <- ms
        ]


arrayToAlist :: Array (Int,Int) Bit -> Alist
arrayToAlist arr = Alist
        { nm        = (n,m)
        , num_nlist = [ length [ () | j <- [1..m], arr ! (i,j) == 1 ]| i <- [1..n]]
        , num_mlist = [ length [ () | i <- [1..n], arr ! (i,j) == 1 ]| j <- [1..m]]
        , nlist     = array (1,n) [ (i,[ j | j <- [1..m], arr ! (i,j) == 1 ])
                                  | i <- [1..n]
                                  ]
        , mlist     = array (1,m) [ (j,[ i | i <- [1..n], arr ! (i,j) == 1 ])
                                  | j <- [1..m]
                                  ]
        }
  where
    ((1,1),(n,m)) = bounds arr

prop_alist a = arrayToAlist (alistToArray a) == a

-----------------------------------



h1 :: M Bit
h1 = matrix
  [ [ 1, 1, 1, 1, 0, 0 ]
  , [ 0, 0, 1, 1, 0, 1 ]
  , [ 1, 0, 0, 1, 1, 0 ]
  ]


h2 :: M Bit
h2 = matrix
  [ [ 1, 1, 1, 1, 0, 0 ]        -- same
  , [ 0, 0, 1, 1, 0, 1 ]        -- same
  , [ 0, 1, 1, 0, 1, 0 ]        -- r2 = r0 XOR r2
  ]

h3 :: M Bit
h3 = matrix
  [ [ 1, 1, 1, 1, 0, 0 ]        -- same
  , [ 0, 1, 1, 0, 1, 0 ]        -- r3
  , [ 0, 0, 1, 1, 0, 1 ]        -- r2
  ]

h4 :: M Bit
h4 = matrix
  [ [ 1, 1, 1, 1, 0, 0 ]
  , [ 0, 1, 1, 0, 1, 0 ]
  , [ 1, 1, 0, 0, 0, 1 ]
  ]

g :: M Bit
g = matrix
  [ [ 1, 0, 0, 1, 0, 1 ]
  , [ 0, 1, 0, 1, 1, 1 ]
  , [ 0, 0, 1, 1, 1, 0 ]
  ]



f :: M Bit -> M Bit -> [Integer]
f h g = map sum $
      [ map fromIntegral $ concat $ fromMatrix (h `mm` columnM (getRowM (rowM (vector $ word) `mm` g)))
      | word <- [ take (rows g) (cycle xs) | xs <- [[0],[1],[0,1],[1,0],[1,0,1],[0,1,0]]]
      ]


stripe :: M Bit -> (Int,Int) -> [Bit]
stripe m (x,y) = [ m ! (x,y) | y <- [y..columns m]]

-- Fix a (finite) stripe, please
--constant :: M Bit -> (Int,Int) -> [Bit] -> [Int]
constant m (x,y) vs | trace (show ("constant",(x,y),vs)) False = undefined
constant m (x,y) vs
  | all (== 0) matches = Nothing        -- all done
  | otherwise = Just $ head' $
        [ x'
        | x' <- [1..rows m]
        , x /= x'               -- XOR by yourself is bad, loses information
        , and $ zipWith (==) (stripe m (x',y)) matches
        ]
  where
        head' (x:_) = x
        head' _ = error $ "head: " ++ show (x,y,vs,matches)
        -- what we are looking for
        matches :: [Bit]
        matches = zipWith xor (stripe m (x,y)) vs

-- xor the 1st and 2nd argument rows, putting the answer in the first argument
xorRow :: Int -> Int -> M Bit -> M Bit
xorRow r1 r2 m = accum xor m
               [ ((r1,y),m ! (r2,y))
               | y <- [1..columns m]
               ]


--g :: M Bit -> M Bit
findG h = h'

  where
     r = matrix [ [ h' ! (x,y)
                   | x <- [1..rows h]
                   ]
                 | y <- take s [1..]
                 ]

     h' :: M Bit
     h' = foldl (\ m (x,y) -> case constant m (x,1 + s) (take y $ unit x) of
                                Nothing -> m
                                Just x' -> xorRow x x' m) h $
          [ (x,y)
          | y <- [1..rows h]
          , x <- [1..rows h]
          , x >= y
          ] ++
          [ (x,y)
          | x <- [1..rows h]
          , y <- [1..rows h]
          , x < y
          ]
     unit x = [ if x==y then 1 else 0 | y <- [1..rows h]]
     s = columns h - rows h

t h = id
        $ [ (x,y)
          | y <- [1..rows h]
          , x <- [1..rows h]
          , x >= y
          ] ++
          [ (x,y)
          | y <- [1..rows h]
          , x <- [1..rows h]
          , x < y
          ]


-- Fix a row
--constantL :: M Bit -> (Int,Int) -> [Bit] -> [Int]       -- Sequence of commands to make this happen

--constantL = undefined

arrayToPPM :: Array (Int,Int) Bit -> String
arrayToPPM arr = "P1\n"
              ++ show (columns arr) ++ " "
              ++ show (rows arr) ++ "\n"
              ++ show (ShowM arr)

-- build the decode matrix out of the Alist matrix.
mk_g :: Alist -> M Bit
mk_g alist = identity (rows m) `beside` m
  where m = alistToArray alist


-- build the parity check matrix,
mk_h :: Alist -> M Bit
mk_h alist = transposeM m `beside` identity (columns m)
  where m = alistToArray alist

t2 s =
     do a <- readAlist "codes/96.3.963"
        let a' = crs $ transposeBM $ a
        putStr $ showBM $ a'
        putStr $ showBM $ head $ findG s $ a'
        -}
