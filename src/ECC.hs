{-# LANGUAGE BangPatterns,RankNTypes, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
-- Simple tester for LDPC-lie things
module ECC where

import System.Random.MWC
import Data.Bit
import Control.Monad
--import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans (MonadIO(liftIO))
import Control.Concurrent
import Data.Word
import qualified Data.Vector.Unboxed  as U
import System.Random.MWC.Distributions

type MessageSize = Int          -- the size of the message
type CodewordSize = Int         -- the size of the message + parity bits
type Rate = Rational

-- By using this, you are restricting what IO operations you commit to do.
newtype EccM a = EccM { runEccM :: GenIO -> IO a }

instance Monad EccM where
    return a = EccM $ \ gen -> return a
    EccM m >>= k = EccM $ \ gen -> do r <- m gen
                                      runEccM (k r) gen

uniformEccM :: Variate a => EccM a
uniformEccM = EccM uniform

standardEccM :: EccM Double
standardEccM = EccM standard

-- basic structure of an error-checking code
data ECC = ECC
     { encode          :: [Bit]       	-> EccM [Bit]
     , txRx            :: [Bit]         -> EccM [Double]
     , decode          :: [Double] 	-> EccM [Bit]         -- ^ (extra info,result)
     , announce        :: Int -> Double -> IO ()              -- ^ give verbal result, passing errors and (computed) BER.
     , message_length  :: MessageSize   -- length of v
     , codeword_length :: CodewordSize  -- length of w
     , verbose         :: Int           -- standard 0 | 1 | 2 | 3 scale:
                                        --   0 = nothing, 1 = on message per message,
                                        --   2 = one message per operation
                                        --   3 = show everything
     }

-- Takes the starting seed, the number of messages, and the encode/decode pair (inside ECC),
-- and returns the BER.
runECC :: Maybe Word32 -> Integer -> ECC -> IO Double
runECC seed count ecc = do
  gen :: GenIO <- case seed of
                    Just s -> initialize (U.singleton s)
                    Nothing -> withSystemRandom $ asGenIO return
  let debug n msg | n <= verbose ecc = putStrLn msg
                  | otherwise        = return ()

  let run !n !errs !ber
       | n == count = do
        debug 2 $ "done, BER = " ++ show ber
        return ber

       | otherwise = do
        debug 2 $ "starting message " ++ show n

        mess0  <- liftM (fmap setBit) $ sequence [ uniform gen | _ <- [1..message_length ecc]]
        debug 2 $ "generated message " ++ show n
        debug 3 $ show mess0

        code0     <- runEccM (encode ecc mess0) gen
        debug 2 $ "encoded message " ++ show n
        debug 3 $ show code0

        rx <- runEccM (txRx ecc code0) gen
        debug 2 $ "tx/rx'd message " ++ show n
        debug 3 $ show rx

        mess1 <- runEccM (decode ecc rx) gen

        when (length mess0 /= length mess1) $ do
                error $ "before and after codeword different lengths" ++ show (length mess0,length mess1)

        let bitErrorCount = length [ () | (b,a) <- zip mess0 mess1, a /= b ]
        debug 1 $ show bitErrorCount ++ " bit errors in message " ++ show n

        let errs' = errs + bitErrorCount
        let ber' = fromIntegral errs' / (fromIntegral (message_length ecc) * (fromIntegral (n + 1)))

        announce ecc bitErrorCount ber'
        debug 2 $ "completed message " ++ show n ++ ", BER = " ++ show ber'

        run (n+1) errs' ber'

  run 0 0 0.0

{-
-- Utilties for building the ECC
generateList :: (PrimMonad m) => Gen (PrimState m) -> Int -> m [Bit]
generateList gen sz = liftM (fmap setBit) $ sequence [ uniform gen | _ <- [1..sz]]

encodeId :: (Monad m) => v Bit -> m (v Bit)
encodeId = return

decodeId :: (Monad m, Functor v, Ord d, Num d) => v d -> m ((),v Bit)
decodeId = return . (,) () . fmap setBit . fmap (>= 0)

checkList :: (Monad m) => [Bit] -> [Bit] -> m Int
checkList xs ys
  | lx /= ly = error $ "internal error in checkList: " ++ show lx ++ " " ++ show ly
  | otherwise = return $ sum $ zipWith (\ x y -> if x == y then 0 else 1) xs ys
  where lx = length xs; ly = length ys

txRxId :: (Monad m, Functor w, Num d) => w Bit -> m (w d)
txRxId = return . fmap (\ x -> if getBit x then 1 else -1)

berForFramesize :: Int -> (Integer -> Integer -> Double)
berForFramesize frameSize frames bitErrors =
  fromIntegral bitErrors / (fromIntegral frameSize * fromIntegral frames)

idECC ::
  (PrimMonad m, MonadIO m) =>
  Int -> m (ECC m [] [] Double ())
idECC frameSize = create >>= \gen -> return $ ECC
  { generate = generateList gen frameSize
  , encode = encodeId
  , txRx   = txRxId
  , decode = decodeId
  , check = checkList
  , ber = berForFramesize frameSize
  , debug = liftIO . putStrLn
  , showX = const Nothing
  , showV = show
  , showW = show
  , showWBool = show
  }
-}

defaultECC :: ECC
defaultECC = ECC
        { encode   = return
        , txRx     = return . fmap (\ x -> if getBit x then 1 else -1)
        , decode   = return . fmap setBit . fmap (>= 0)
        , announce = \ _ ber -> putStrLn $ "BER: " ++ show ber
        , message_length  = 16
        , codeword_length = 16
        , verbose = 0
        }

txRx_EbN0 :: Double -> ECC -> ECC -- [Bit] -> EccM [Double]
txRx_EbN0 ebnoDB ecc = ecc { txRx = \ xs -> do
        rs :: [Double]  <- sequence [ standardEccM | _ <- [1..length xs] ]
        return $ zipWith (+) (fmap (* sqrt var) rs) $ fmap (\ x -> if getBit x then 1 else -1) $ xs }
  where
         rate = rateOf ecc
         var = 1/(2 * fromRational rate * (10 **(ebnoDB/10)))

rateOf :: ECC -> Rational
rateOf ecc = fromIntegral (message_length ecc) / fromIntegral (codeword_length ecc)

hard :: (Num a, Ord a) => a -> Bit
hard = setBit . (> 0)

soft :: (Num a) => Bit -> a
soft 0 = -1
soft 1 = 1

{-
main :: IO ()
main = do
        let ebN0s = [0,2..8]
        xs <- sequence [ do ber <- runECC Nothing 100000 defaultECC { txRx = txRx_EbN0 1 ebN0 , announce = \ _ _ -> return () }
                            print $ ber * 16 * 100000
                            return ber
                       | ebN0 <- ebN0s
                       ]
        putStrLn $ unlines [ show (v,n) | (v,n) <- zip ebN0s xs ]
        return ()
-}
