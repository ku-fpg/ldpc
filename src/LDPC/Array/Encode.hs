module LDPC.Array.Encode where

import Data.Array.Matrix
import Data.Bit

encoder :: M Bit -> V Bit -> V Bit
encoder g v = getRowM $ rowM v `mm` g


