-- This is the format defined in
-- http://www.inference.phy.cam.ac.uk/mackay/codes/alist.html

module Data.Alist where

class Alist a where
  readAlist  :: String -> IO a
  writeAlist :: String -> a -> IO ()

