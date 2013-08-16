module Codes.Wikipedia where

import Data.ListMatrix

-- At one point these codes were not working because h had not yet been
-- transformed (via elementary row operations) to have an identity matrix on
-- the right side; the derivation is on the Wikipedia page

-- From http://en.wikipedia.org/wiki/Low-density_parity-check_code
-- This is a (6, 3) linear code, with n = 6 and k = 3.
h_6_3 :: ListMatrix Bool
h_6_3 = fmap (/= 0) $ listMatrix
--  [ [ 1, 1, 1, 1, 0, 0 ]
--  , [ 0, 0, 1, 1, 0, 1 ]
--  , [ 1, 0, 0, 1, 1, 0 ]
--  ]
  [ [ 1, 1, 1, 1, 0, 0 ]
  , [ 0, 1, 1, 0, 1, 0 ]
  , [ 1, 1, 0, 0, 0, 1 ]
  ]

g_6_3 :: ListMatrix Bool
g_6_3 = fmap (/= 0) $ listMatrix
  [ [ 1, 0, 0, 1, 0, 1 ]
  , [ 0, 1, 0, 1, 1, 1 ]
  , [ 0, 0, 1, 1, 1, 0 ]
  ]
