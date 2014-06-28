{-# LANGUAGE InstanceSigs #-}

import Control.Monad
import Data.Maybe
import Data.Char

newtype MaybeT m a = MaybeT { runMaybeT  :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
	return = MaybeT . return . Just
	-- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
	--x >>= f = MaybeT $ runMaybeT x >>= apply where
	--	apply Nothing = return Nothing 
	--	apply (Just val) = runMaybeT $ f val
	x >>= f = MaybeT $
		runMaybeT x >>= \x' ->
			if isJust x' then runMaybeT $ f (fromJust x') else return Nothing


instance Monad m => MonadPlus (MaybeT m) where
	mzero = MaybeT . return $ Nothing
	mplus x y = MaybeT $ do
		x' <- runMaybeT x
		if isJust x' then return x' else runMaybeT y -- lazy: first left then right


class MonadTrans t where
	lift :: (Monad m) => m a -> t m a


instance MonadTrans MaybeT where
	lift = MaybeT . liftM Just -- IO String -> MaybeT IO (Maybe String)

isValid :: String -> Bool
isValid s = length s >= 8 && any isAlpha s && any isNumber s && any isPunctuation s

getInput :: MaybeT IO String
getInput = do
	input <- lift getLine
	MaybeT $ return (if isValid input then Just input else Nothing)



askPassphrase :: IO String
askPassphrase = do
	putStrLn "Enter your Input"
	v <- runMaybeT $ msum $ getInput:repeat ((lift . putStrLn $ "Invalid Input, enter another Input again:") >>= const getInput)
	return $ fromJust v -- we are sure v is Just because of msum

	--runMaybeT getInput >>= maybe (putStrLn "Again" >>= const askPassphrase) return

main :: IO ()
main = do 
	pass <- askPassphrase
	putStrLn $ "Valid Input is " ++ pass
	--pass <- runMaybeT getInput
	--if isJust pass then putStrLn $ "Valid! " ++ fromJust pass else 
	--	putStrLn "Invalid!" >>= const main