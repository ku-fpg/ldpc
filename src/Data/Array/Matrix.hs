module Data.Array.Matrix where

import Data.Array
import Data.List
import Numeric

type V a = Array Int a          -- We assume that we index (1,length array)
type M a = Array (Int,Int) a    -- We assume that we index ((1,1),(length A,length B))

rowM :: V a -> M a
rowM a = ixmap ((1,li),(1,ul)) (\ (_,i) -> i) a
   where
         (li,ul) = bounds a

columnM :: V a -> M a
columnM a = ixmap ((li,1),(ul,1)) (\ (i,_) -> i) a
   where
         (li,ul) = bounds a

rows :: M a -> Int
rows = fst . snd . bounds

columns :: M a -> Int
columns = snd . snd . bounds

getRowM :: M a -> V a
getRowM a = vector $ elems a
  where
    ((1,1),(1,_)) = bounds a

getColumnM :: M a -> V a
getColumnM a = vector $ elems a
  where
    ((1,1),(_,1)) = bounds a

transposeM :: M a -> M a
transposeM m = array ((1,1),(columns m,rows m)) [ ((y,x),v) | ((x,y),v) <- assocs m ]

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
matrix xs = listArray ((1,1),(length xs,length (head xs))) $ concat xs

vector :: [a] -> V a
vector xs = listArray (1,length xs) xs

fromVector :: V a -> [a]
fromVector = elems

fromMatrix :: M a -> [[a]]
fromMatrix a =  [ [ a ! (i,j) | j <- [lj .. uj] ]
                | i <- [li .. ui ]
                ]
  where ((li,lj),(ui,uj)) =  bounds a


data ShowM a = ShowM (M a)

instance Show a => Show (ShowM a) where
   show (ShowM a) =
           unlines [ unwords [ take (w - length x) (repeat ' ') ++ x
                             | (w,x) <- ws `zip` xs
                             ]
                   | xs <- xss
                   ]
      where
         ((li,lj),(ui,uj)) =  bounds a
         xss = [ [ show (a ! (i,j)) | j <- [lj .. uj] ]
               | i <- [li .. ui ]
               ]
         ws = map maximum (map (map length) (transpose xss))

data ShowV a = ShowV (V a)
instance Show a => Show (ShowV a) where
   show (ShowV a) = show [ a ! j | j <- [lj .. uj] ]
      where
         (lj,uj) =  bounds a
-- Trick: use ShowM (<some array>) to get better Showing of something.

data E a = E a

instance RealFloat a => Show (E a) where
   showsPrec _ (E a) = showEFloat (Just 2) a

data F a = F a

instance RealFloat a => Show (F a) where
   showsPrec _ (F a) = showFFloat (Just 2) a

data S = S String

instance Show S where
  show (S str) = str

identity :: Num a => Int -> M a
identity n = matrix [ [ if x == y then 1 else 0
                      | x <- [1..n]
                      ] | y <- [1..n]
                    ]

beside :: M a -> M a -> M a
beside m1 m2 = matrix $ transpose (transpose (fromMatrix m1) ++ transpose (fromMatrix m2))

splice :: ((Int,Int),(Int,Int)) -> M a -> M a
splice bds m = array ((1,1),(1 + ui - li,1 + uj - lj)) [ ((1 + i-li,1 + j - lj),v) | ((i,j),v) <- assocs m, bds `inRange` (i,j) ]
  where
          ((li,lj),(ui,uj)) = bds
