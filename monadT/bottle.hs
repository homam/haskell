{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoMonomorphismRestriction #-}

--import Control.Monad.Trans.State.Lazy (State)
import Control.Monad.State
--import Debug.Trace


bottle' :: State Int String
bottle' = do
	t <- getS
	putS (t - 1)
	return $ show (t - 1)

countBottles' :: State Int ()
countBottles' = do
	n <- bottle'
	unless (n == "0") countBottles'

putS :: MonadState s m => s -> m ()
putS newState = state $ const ((), newState)

getS :: MonadState a m => m a
getS = state $ \st -> (st, st)

bottle :: StateT Int IO Int
bottle = do
	t <- get
	liftIO $ putStrLn $ show t ++ " bottles of beer on the wall!"
	put (t - 1)
	return (t - 1)

countBottles :: StateT Int IO ()
countBottles = do
	n <- bottle
	if n == 0 then liftIO $ return () else countBottles

bottles :: Int -> IO ((), Int)
bottles = runStateT countBottles

main :: IO ()
main = do 
	(_, i) <- bottles 10
	print i
	let n = runState countBottles' 10
	print n 