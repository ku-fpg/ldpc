{-# LANGUAGE BangPatterns, RankNTypes, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module Haskell.Array.Decode where

import Haskell.Array.Sig
import Data.Bit
import Data.Array
import ECC

decoder :: Int -> M Bit -> V Double -> V Bit
decoder = ldpc

ldpc :: (Floating d, Ord d) => Int -> M Bit -> V d -> V Bit
ldpc maxIterations a orig_lam = fmap hard $ loop 0 ne orig_lam
  where
    ne = fmap (const 0) a

    loop !n ne lam
        | null [ () | 1 <- elems ans ] = lam
        | n >= maxIterations           = orig_lam
        | otherwise                    = loop (n+1) ne' lam'
      where
        c_hat :: V Bit
        c_hat = fmap hard lam

        ans :: M Bit
        ans = a `mm` columnM c_hat

        ne' = ne // [ ((m,n), -2 * atanh (product
                         [ tanh (- ((lam ! j - ne ! (m,j)) / 2))
                         | j <- indices lam
                         , j /= n
                         , a ! (m,j) == 1
                         ]))
                    | (m,n) <- indices ne
                    , a ! (m,n) == 1
                    ]

        lam' = accum (+) orig_lam [ (n,a) | ((_,n),a) <- assocs ne' ]


