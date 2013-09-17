module Main where

import ECC
import Data.Array.Matrix
import Data.Char
import qualified Manifold.Array as A
import System.IO
import Stats

pickECC :: [String] -> IO ECC
--pickECC "array-moon:200" = A.moon_array_ecc 10

pickECC ["array",code,ns] | all isDigit ns = A.array_ecc' code (read ns)
--pickECC ["min_array","moon",ns] | all isDigit ns = A.min_moon_array_ecc (read ns)
pickECC ["bpsk"]               = return defaultECC

usage = "ldpc <messages> <code1> <code2> <EbN0_1> <EbN0_2> <EbN0_3> <EbN0_4> .."

splitCodename :: String -> [String]
splitCodename = words . map (\ c -> if c == '/' then ' ' else c)

main :: IO ()
main = do
        print "Hello"
        let dbs = [8] -- 0,2,4,6] --- [0..8]
        let codes = ["bpsk"] ++
--                    [ "array/moon.7.13/"++ show n | n <- [16] ] ++
                    [ "array/moon.7.13/"++ show n | n <- [128] ] ++
--                    [ "min_array/moon/"++ show n | n <- [16] ] ++
                    []
        xs <- sequence
               [ sequence
                  [ do ecc0 <- pickECC (splitCodename nm)
                       let ecc1 = txRx_EbN0 db ecc0
                       let ecc2 = ecc1 { verbose = 0 , announce = \ n _ ->
                                                                if n == 0 then return False
                                                                          else do putStr (show n ++ " ")
                                                                                  hFlush stdout
                                                                                  return False }
--                       ecc3 <- stopAfter 1000 ecc2
                       let loop n c = do
                             print ("loop",c,n)
                             res <- sequence [ runECC Nothing 1000 ecc2
                                             | _ <- [1..n]
                                             ]

--                                 (c0,ber) <- runECC Nothing n ecc2
--                             print ("returned",c0,ber)
                             let c' = sum (map fst res) + c
                             if c' > 100
                               then return (map snd res)
                               else do bers <- loop (n * 2) c'
                                       return $ map snd res ++ bers
--                       print (nm,db)
                       bers <- loop 1 0
                       print bers
                       ber <- nintyFifth bers
                       print ber
                       return $ ber
                  | db <- dbs
                  ]
               | nm <- codes
               ]
        let showEst e = show (E $ estPoint e) ++ " " ++ show (E (estLowerBound e)) ++ " " ++ show  (E (estUpperBound e))
        let xs1 = zipWith (:) (fmap S codes) (fmap (fmap (S . showEst)) xs)
        let xs2 = (S "code \\ ebN0" : map (S . show . F) dbs) : xs1
        let m = matrix xs2
        print (ShowM (fmap (\ (S a) -> (S $ a ++ "\t")) m))



