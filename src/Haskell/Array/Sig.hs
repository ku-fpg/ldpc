module Haskell.Array.Sig where

import Data.Array

type V a = Array Int a
type M a = Array (Int,Int) a

rowM :: V a -> M a
rowM a = ixmap ((0,li),(0,ul)) (\ (_,i) -> i) a
   where
         (li,ul) = bounds a

columnM :: V a -> M a
columnM a = ixmap ((li,0),(ul,0)) (\ (i,_) -> i) a
   where
         (li,ul) = bounds a

rows :: M a -> Int
rows = (+ 1) . fst . snd . bounds

columns :: M a -> Int
columns = (+ 1) . snd . snd . bounds

getRowM :: M a -> V a
getRowM a = vector $ elems a
  where
    ((0,0),(0,_)) = bounds a

mm :: Num a => M a -> M a -> M a
mm = matMult

-- From the Haskell report
matMult :: (Ix a, Ix b, Ix c, Num d) => Array (a,b) d -> Array (b,c) d -> Array (a,c) d
matMult x y =  array resultBounds
                         [((i,j), sum [x!(i,k) * y!(k,j) | k <- range (lj,uj)])
                                       | i <- range (li,ui),
                                         j <- range (lj',uj') ]
        where ((li,lj),(ui,uj))         =  bounds x
              ((li',lj'),(ui',uj'))     =  bounds y
              resultBounds
                | (lj,uj)==(li',ui')    =  ((li,lj'),(ui,uj'))
                | otherwise             = error "matMult: incompatible bounds"

matrix :: [[a]] -> M a
matrix xs = listArray ((0,0),(length xs - 1,length (head xs) - 1)) $ concat xs

vector :: [a] -> V a
vector xs = listArray (0,length xs - 1) xs

fromVector :: V a -> [a]
fromVector = elems

-- WRONG: TO FIX
fromMatrix :: M a -> [[a]]
fromMatrix = (:[]) . elems


data ShowM a = ShowM (M a)

instance Show a => Show (ShowM a) where
   show (ShowM a) = unlines [ show [ a ! (i,j) | j <- [lj .. uj] ]
                            | i <- [li .. ui ]
                            ]
      where
         ((li,lj),(ui,uj)) =  bounds a

data ShowV a = ShowV (V a)
instance Show a => Show (ShowV a) where
   show (ShowV a) = show [ a ! j | j <- [lj .. uj] ]
      where
         (lj,uj) =  bounds a
-- Trick: use ShowM (<some array>) to get better Showing of something.


