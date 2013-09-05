module Haskell.Array.Encode where

import Haskell.Array.Sig
import Data.Bit

encoder :: M Bit -> V Bit -> V Bit
encoder g v = getRowM $ rowM v `mm` g


