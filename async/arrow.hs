-- import Control.Arrow
import Control.Monad ((>=>))

type Kleisli m a b = a -> m b

arr :: Monad m => (a -> b) -> Kleisli m a b -- a -> m b
arr f = return . f -- == fmap f . return  -- if Functor m =>

-- 		 Monad m => (a -> m b)    -> (b -> m c)    -> (a -> m c)
(>>>) :: Monad m => Kleisli m a b -> Kleisli m b c -> Kleisli m a c
(f >>> g) a = do  -- == f >=> g
	b <- f a
	g b

count :: String -> FilePath -> IO Int  -- Main.Kleisli IO FilePath Int
count w = readFile >=>
	arr words >>> arr (filter (==w)) >>> arr length

main :: IO ()
main = do 
	i <- count "m" "./arrow.hs" --print $ first (first (*2)) ((4::Integer,"A"),"B")
	print i