import Control.Applicative

main :: IO ()
main = do 
	--line <- fmap reverse getLine
	--putStrLn line

	print $ map (\f -> f "ad") $ fmap(++) ["db","A"]

	print $  [\f -> f 2, \f -> f 3] <*> [\x -> 10 + x, \x -> 20 + x]

	print $ [(1+),(2+),(3+) ] <*> [1,2,3]

	print $ [(*)] <*> [1..3] <*> [1..3]
	print $  (*) `map` [1..3] <*> [1..3]

	print $ sequenceA [(\x -> 2 * x), (\x -> 3 * x), (\x -> 4 * x)] 6

	print $ sequenceA' [(\x -> 2 * x), (\x -> 3 * x), (\x -> 4 * x)] 6


sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = liftA2 (:) x (sequenceA xs)

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (liftA2 (:)) (pure [])

sequenceA'' :: (Applicative f) => [f a] -> f [a]
sequenceA'' [] = pure []
sequenceA'' (x:xs) = (:) <$> x <*> sequenceA xs