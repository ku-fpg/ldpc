module Main where

import ECC
import Haskell.Array.Sig
import Data.Char
import qualified Manifold.Array as A
import System.IO

pickECC :: [String] -> ECC
--pickECC "array-moon:200" = A.moon_array_ecc 10

pickECC ["array","moon",ns] | all isDigit ns = A.moon_array_ecc (read ns)
pickECC ["min_array","moon",ns] | all isDigit ns = A.min_moon_array_ecc (read ns)
pickECC ["bpsk"]               = defaultECC


usage = "ldpc <messages> <code1> <code2> <EbN0_1> <EbN0_2> <EbN0_3> <EbN0_4> .."

splitCodename :: String -> [String]
splitCodename = words . map (\ c -> if c == '/' then ' ' else c)

main :: IO ()
main = do
        print "Hello"
        let dbs = [2] --- [0..8]
        let codes = ["bpsk"] ++
                    [ "array/moon/"++ show n | n <- [16] ] ++
                    [ "min_array/moon/"++ show n | n <- [16] ] ++
                    []
        xs <- sequence
               [ sequence
                  [ do let ecc0 = pickECC (splitCodename nm)
                       let ecc1 = txRx_EbN0 db ecc0
                       let ecc2 = ecc1 { verbose = 0 , announce = \ n _ ->
                                                                if n == 0 then return False
                                                                          else do putStr (show n ++ " ")
                                                                                  hFlush stdout
                                                                                  return False }
--                       ecc3 <- stopAfter 1000 ecc2
                       let loop n c = do
                             print ("loop",c,n)
                             (c0,ber) <- runECC Nothing n ecc2
                             print ("returned",c0,ber)
                             if c + c0 > 1000
                               then return [ber]
                               else do bers <- loop (n * 2) (c  + c0)
                                       return $ ber : bers
                       print (nm,db)
                       bers <- loop 10000 0
                       print bers
                       let muls = take (length bers) (iterate (*2) 1) :: [Double]
                       print muls
                       let ber = sum (zipWith (*) bers muls) / sum muls
                       print ber
                       return $ ber
                  | db <- dbs
                  ]
               | nm <- codes
               ]
        let xs1 = zipWith (:) (fmap S codes) (fmap (fmap (S . show . E)) xs)
        let xs2 = (S "code \\ ebN0" : map (S . show . F) dbs) : xs1
        let m = matrix xs2
        print (ShowM (fmap (\ (S a) -> (S $ a ++ "\t")) m))


