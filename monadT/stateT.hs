{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Control.Monad.State hiding (StateT, State, runState, runStateT)


newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
	return a = State $ \s -> (a, s) 
	g >>= f = State $ \s ->
		let (a, s') = runState g s
		in runState (f a) s'

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) } -- deriving (Eq, Show)

instance Monad m => Monad (StateT s m) where
	return a = StateT $ \s -> return (a, s)
	g >>= f = StateT $ \s -> do
		(a, s') <- runStateT g s
		runStateT (f a) s'


instance MonadTrans (StateT s) where
	-- IO x -> StateT IO (State x)
	lift m = StateT $ \s -> do
		a <- m
		return (a, s)


instance (Monad m) => MonadState s (StateT s m) where
	get = StateT $ \s -> return (s, s)
	put s = StateT $ \_ -> return ((), s)



instance (MonadPlus m) => MonadPlus (StateT s m) where
	mzero = StateT $ const mzero
	StateT x1 `mplus` StateT x2 = StateT $ \s -> x1 s `mplus` x2 s



main :: IO ()
main = print "hello"