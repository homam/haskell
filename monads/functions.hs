import Control.Monad
import Control.Monad.Writer

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) 
	| f x  = x : filter' f xs
	| otherwise = filter' f xs


filterM' :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' f (x:xs) = do
	v <- f x
	rs <- filterM' f xs
	return (if v then x:rs else rs)



fold' :: (a -> b -> a) -> a -> [b] -> a
fold' _ a [] = a
fold' f a (x:xs) = fold' f (f a x) xs

foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ a [] = return a
foldM' f a (x:xs) = do
	a' <- f a x
	foldM' f a' xs

main :: IO ()
main = do
	print "--filter--"
	print $ filter' odd [1..10 :: Integer]
	print $ filterM' (\x -> [odd x]) [1..10 :: Integer]
	print $ filterM' (const [True,False]) [1..2 :: Integer]
	print "--fold--"
	print $ fold' (+) 0 [1..10 :: Integer]
	print $ runWriter $ foldM' (\acc a -> writer (acc+a, [a+acc])) 0 [1..10 :: Integer]
