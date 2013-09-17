module Haskell.Encode where

import Haskell.ArraySig

encoder :: M Bool -> V Bool -> V Bool
encoder g v = multVM "encode" v g
