{-# LANGUAGE BangPatterns, RankNTypes, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module LDPC.Array.Decode where

import Prelude hiding (abs)

import Data.Function (fix)

import Data.Array.Matrix
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
        {-# NOINLINE c_hat #-}
        c_hat :: V Bit
        c_hat = fmap hard lam

        {-# NOINLINE ans #-}
        ans :: M Bit
        ans = a `mm` columnM c_hat

        {-# NOINLINE ne' #-}
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

        {-# NOINLINE lam' #-}
        lam' :: V d
        lam' = accum (+) orig_lam [ (n,a) | ((_,n),a) <- assocs ne' ]


{-# RULES "SparseToFrom" forall a sm. toSparse a (frSparse a sm) = sm #-}
{-# RULES "SparseFromTo" forall a dm. frSparse a (toSparse a dm) = dm #-}

{-# "Nick" forall a ne f. toSparse (frSparse a ne // [ (idx,f idx) | idx <- indices (frSparse a ne), a!idx == 1 ])
                                = SM.mapWithIndex a f ne #-}

toSparse :: M Bit -> M a -> V a
toSparse = undefined -- uses the first argument as a mask

frSparse :: M Bit -> V a -> M a
frSparse = undefined

rep :: M Bit -> (M d -> V d -> V d) -> (V d -> V d -> V d)
rep a h sm = h (frSparse a sm)

abs :: M Bit -> (V d -> V d -> V d) -> (M d -> V d -> V d)
abs a h m = h (toSparse a m)

-- min_decoder :: Int -> M Bit -> V Double -> V Bit
-- min_decoder = min_ldpc

-- min_ldpc :: forall d . (Floating d, Ord d) => Int -> M Bit -> V d -> V Bit
-- min_ldpc maxIterations a orig_lam = fmap hard $ loop 0 orig_ne orig_lam
--   where
--     orig_ne :: M d
--     orig_ne = fmap (const 0) a

--     loop :: Int -> M d -> V d -> V d
--     loop !n ne lam
--         | all (== 0) (elems ans)       = lam
--         | n >= maxIterations           = orig_lam
--         | otherwise                    = loop (n+1) ne' lam'
--       where
--         c_hat :: V Bit
--         c_hat = fmap hard lam

--         ans :: M Bit
--         ans = a `mm` columnM c_hat

--         ne' :: M d
--         ne' = ne // [ ((m,n), -0.75 * foldr1 min'
--                          [ - (lam ! j - ne ! (m,j))
--                          | j <- indices lam
--                          , j /= n
--                          , a ! (m,j) == 1
--                          ])
--                     | (m,n) <- indices ne
--                     , a ! (m,n) == 1
--                     ]

--         lam' :: V d
--         lam' = accum (+) orig_lam [ (n,a) | ((_,n),a) <- assocs ne' ]

-- min' :: (Num a, Ord a) => a -> a -> a
-- min' x y = signum x * signum y * min (abs x) (abs y)
