import Control.Monad.Writer

example0 :: (Int, String)
example0 = runWriter (return 3 :: Writer String Int)

example1 :: (Int, [String])
example1 = runWriter $ logX 3 >>= \x -> logX 5 >>= \y -> writer (x*y, ["Product"]) -- return (x*y)

logX :: (Show x) => x -> Writer [String] x 
logX x = writer (x, ["Got " ++ show x])

example2 :: Int -> Int -> Writer [String] Int
example2 a b = do
	a' <- logX a
	b' <- logX b
	tell ["Product"]
	return (a' * b')
	-- writer (a' * b', ["Product"])


gcd' :: Int -> Int -> Writer [String] Int
gcd' a b 
	| b == 0 = writer (a, ["GCD = " ++ show a])
	| otherwise = do 
		tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
		gcd' b (a `mod` b)

main :: IO ()
main = do
	print example0
	print example1
	print $ runWriter $ example2 3 5
	print $ runWriter $ gcd' (3*4*5*7) (2*8*9)
	mapM_ putStrLn $ snd $ runWriter $ gcd' (3*4*5*7) (2*8*9)
	