{-# LANGUAGE TypeFamilies #-}
module Data.ListMatrix where

import Data.List
import Data.Monoid

listMatrix :: [[a]] -> ListMatrix a
listMatrix xss
  | or (zipWith (/=) (map length xss) (tail (map length xss)))
                         = error "bad length in list matrix"

  | otherwise = ListMatrix xss

newtype ListMatrix a = ListMatrix {unListMatrix :: [[a]]}
  deriving (Eq, Ord)

instance Functor ListMatrix where
  fmap f (ListMatrix xss) = ListMatrix $ fmap (fmap f) xss

instance Show a => Show (ListMatrix a)
  where
    show (ListMatrix xss) = unlines
                [ unwords
                   [ rjust w (show x)
                   | (x,w) <- xs `zip` widths
                   ]
                | xs <- xss
                ]
      where
        rjust w x = take (w - length x) (repeat ' ') ++ x
        widths    = map (maximum . map (length . show)) $ transpose xss


mm :: (Num a) => ListMatrix a -> ListMatrix a -> ListMatrix a
mm (ListMatrix m1) (ListMatrix m2)
   | length (head m1) /= length m2 = error "matrixes do not match"
   | otherwise = ListMatrix m3
  where
    m3 = [ [ sum (zipWith (*) a b) | b <- transpose m2 ] | a <- m1 ]

boolMM :: (a ~ Bool) => ListMatrix a -> ListMatrix a -> ListMatrix a
boolMM (ListMatrix m1) (ListMatrix m2)
   | length (head m1) /= length m2 = error $ "matrixes do not match" ++ show (length (head m1), length m2)
   | otherwise = ListMatrix m3
  where
    m3 = [ [ foldr (/=) False (zipWith (&&) a b) | b <- transpose m2 ] | a <- m1 ]

identity :: Num a => Int -> ListMatrix a
identity n = ListMatrix [ [ if x == y then 1 else 0 | x <- [1..n] ] | y <- [1..n]]
