{-# LANGUAGE BangPatterns,RankNTypes #-}
-- Simple tester for LDPC-lie things
module ECC where

import System.Random.MWC
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans (MonadIO(liftIO))

type FrameSize = Int

-- basic structure of an error-checking code
data ECC m v w d x = ECC
     { generate  ::                        m (v Bool)
     , encode    :: v Bool		-> m (w Bool)
     , txRx      :: w Bool		-> m (w d)
     , decode    :: w d 	        -> m (x,v Bool) -- ^ (extra info,result)
     , check     :: v Bool -> v Bool    -> m Int
     , ber       :: Integer -> Integer  -> Double               -- ^ given this many frames, and this number of bit errors, what is the BER?
     , debug     :: String              -> m ()
     , showX     :: x -> Maybe String
     , showV     :: v Bool -> String
     , showW     :: w d -> String
     , showWBool :: w Bool -> String
     }

noDebug :: Monad m => String -> m ()
noDebug _ = return ()

-- returns the number of bits transmitted, and bits recieved intact.
runECC :: Monad m => ECC m v w d x -> (a -> x -> a) -> a -> Integer -> m (a,Integer)
runECC ecc combine zero count = run 0 0 zero
  where
    run !n !errs !acc
     | n == count = do
        debug ecc $ "returning " ++ show errs
        return (acc,errs)
     | otherwise = do
        debug ecc $ "starting packet " ++ show n

        code0     <- generate ecc
        debug ecc $ showV ecc code0

        code1     <- encode ecc code0
        debug ecc $ showWBool ecc code1

        rx        <- txRx ecc code1
        debug ecc $ showW ecc rx

        (extra_info,code2)     <- decode ecc rx
        maybe (return ()) (debug ecc) $ showX ecc extra_info
        debug ecc $ showV ecc code2

        bitErrors <- check ecc code0 code2
        debug ecc $ "errors " ++ show bitErrors
        run (n+1) (errs + toInteger bitErrors) (combine acc extra_info)


-- Utilties for building the ECC
generateList :: (PrimMonad m) => Gen (PrimState m) -> Int -> m [Bool]
generateList gen sz = sequence [ uniform gen | _ <- [1..sz]]

encodeId :: (Monad m) => v Bool -> m (v Bool)
encodeId = return

decodeId :: (Monad m, Functor v) => v Double -> m ((),v Bool)
decodeId = return . (,) () . fmap (>= 0)

checkList :: (Monad m) => [Bool] -> [Bool] -> m Int
checkList xs ys
  | lx /= ly = error $ "internal error in checkList: " ++ show lx ++ " " ++ show ly
  | otherwise = return $ sum $ zipWith (\ x y -> if x == y then 0 else 1) xs ys
  where lx = length xs; ly = length ys

txRxId :: (Monad m, Functor w) => w Bool -> m (w Double)
txRxId = return . fmap (\ x -> if x then 1 else -1)

berForFramesize :: Int -> (Integer -> Integer -> Double)
berForFramesize frameSize frames bitErrors =
  fromIntegral bitErrors / (fromIntegral frameSize * fromIntegral frames)

mkECCId ::
  (PrimMonad m, MonadIO m) =>
  Int -> m (ECC m [] [] Double ())
mkECCId frameSize = create >>= \gen -> return $ ECC
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

--main :: IO ()
--main = mainWith (mkECCId 32)

mainWith :: (MonadIO m,Show a) => (a -> x -> a) -> a -> Integer -> ECC m v w d x -> m ()
mainWith combine zero frames ecc = do
  (a,errs) <- runECC ecc combine zero frames
  liftIO $ print a
  liftIO $ putStrLn $ "BER = " ++ show (ber ecc frames errs)
