import Control.Monad

newtype ListT m a = ListT { runListT :: m [a] } -- deriving (Eq, Show)

instance Monad m => Monad (ListT m) where
	return a = ListT . return $ [a]
	ma >>= f = ListT $ do
		as <- runListT ma
		fs <- runListT $ mapM f as
		return $ join fs

class MonadTrans t where
	lift :: (Monad m) => m a -> t m a

instance MonadTrans ListT where
	-- IO x -> ListT IO [x]
	lift =  ListT . liftM (: [])

main :: IO ()
main = print "hello"