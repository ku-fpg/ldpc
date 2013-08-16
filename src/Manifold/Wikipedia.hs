module Manifold.Wikipedia where

--import Data.ListMatrix
--import Codes.Wikipedia
--import Haskell.Encode
import ECC

genECC :: Monad m => IO (ECC m [] [] Double ())
genECC = error "Manifold.Wikipedia.genECC is not quite ready yet"
{-ECC
    { encode = return . encoder g_6_3
    }
        let frameSize = 32
        let frames = 100
        gen <- create
        let ecc = ECC
                { generate = generateList gen frameSize
                , encode = encodeId
                , txRx   = txRxId
                , decode = decodeId
                , check = checkList
                , ber = berForFramesize frameSize
                , debug = putStrLn
                }
        errs <- runECC ecc frames
-}
