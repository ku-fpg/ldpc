{-# LANGUAGE BangPatterns, RankNTypes, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module Haskell.Array.Decode where

import Haskell.Array.Sig
import Data.Bit
import Data.Array
import ECC

decoder :: Int -> M Bit -> V Double -> V Bit
decoder = ldpc

ldpc :: forall d. (Floating d, Ord d) => Int -> M Bit -> V d -> V Bit
ldpc maxIterations a orig_lam = fmap hard $ loop 0 orig_ne orig_lam
  where
    orig_ne :: M d
    orig_ne = fmap (const 0) a

    loop :: Int -> M d -> V d -> V d
    loop !n ne lam
        | all (== 0) (elems ans)       = lam
        | n >= maxIterations           = orig_lam
        | otherwise                    = loop (n+1) ne' lam'
      where
        c_hat :: V Bit
        c_hat = fmap hard lam

        ans :: M Bit
        ans = a `mm` columnM c_hat

        ne' :: M d
        ne' = ne // [ ((m,n), -2 * atanh (product
                         [ tanh (- ((lam ! j - ne ! (m,j)) / 2))
                         | j <- indices lam
                         , j /= n
                         , a ! (m,j) == 1
                         ]))
                    | (m,n) <- indices ne
                    , a ! (m,n) == 1
                    ]

        lam' :: V d
        lam' = accum (+) orig_lam [ (n,a) | ((_,n),a) <- assocs ne' ]

min_decoder :: Int -> M Bit -> V Double -> V Bit
min_decoder = min_ldpc

min_ldpc :: forall d . (Floating d, Ord d) => Int -> M Bit -> V d -> V Bit
min_ldpc maxIterations a orig_lam = fmap hard $ loop 0 orig_ne orig_lam
  where
    orig_ne :: M d
    orig_ne = fmap (const 0) a

    loop :: Int -> M d -> V d -> V d
    loop !n ne lam
        | all (== 0) (elems ans)       = lam
        | n >= maxIterations           = orig_lam
        | otherwise                    = loop (n+1) ne' lam'
      where
        c_hat :: V Bit
        c_hat = fmap hard lam

        ans :: M Bit
        ans = a `mm` columnM c_hat

        ne' :: M d
        ne' = ne // [ ((m,n), -0.75 * foldr1 min'
                         [ - (lam ! j - ne ! (m,j))
                         | j <- indices lam
                         , j /= n
                         , a ! (m,j) == 1
                         ])
                    | (m,n) <- indices ne
                    , a ! (m,n) == 1
                    ]

        lam' :: V d
        lam' = accum (+) orig_lam [ (n,a) | ((_,n),a) <- assocs ne' ]

min' :: (Num a, Ord a) => a -> a -> a
min' x y = signum x * signum y * min (abs x) (abs y)
