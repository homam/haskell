{-# LANGUAGE InstanceSigs, TypeSynonymInstances, Trustworthy, PolyKinds, GADTs #-}
import Control.Monad ((>=>))

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }


class Arrow arr where
	
	arr :: (a -> b) -> arr a b
	
	(>>>) :: arr a b -> arr b c -> arr a c

	(***) :: arr b c -> arr b' c' -> arr (b,b') (c,c')
	f *** g = first f >>> second g

	(&&&) :: arr b c -> arr b c' -> arr b (c,c')
	f &&& g = arr (\x->(x,x)) >>> (f *** g)
	
	first :: arr b c -> arr (b, d) (c, d) -- == first f = f *** arr id
	
	second :: arr b c -> arr (d, b) (d, c)
	second f = arr swap >>> first f >>> arr swap where
		swap (x, y) = (y, x)

	--f &&& g = arr (\b -> (b,b)) >>> f *** g

instance Arrow (->) where
	arr = id
	(>>>) = flip (.)
	(f &&& g) x = (f x, g x) -- == f &&& g = \x -> (f x, g x)
	(f *** g) (x, y)  = (f x, g y) -- == \(x, y) -> (f x, g y)
	first f (a, c) = (f a, c) 

---- 

instance Monad m => Arrow (Kleisli m) where
	arr f = Kleisli (return . f)
	(Kleisli f') >>> (Kleisli g') = Kleisli (f' >=> g')
		--let f' = runKleisli f
		--let g' = runKleisli g
	(Kleisli f) &&& (Kleisli g) = Kleisli $ \x -> do
		fx <- f x
		gx <- g x
		return (fx, gx)

	(Kleisli f) *** (Kleisli g) = Kleisli $ \(x, y) -> do
		fx <- f x
		gy <- g y
		return (fx, gy)

	first (Kleisli f) = Kleisli $ \ (b,d) -> do 
		c <- f b
		return (c, d)



count :: String -> Kleisli IO FilePath Int
count w = Kleisli readFile >>>
	arr words >>> arr (filter (==w)) >>> arr length

-----

newtype SF a b = SF { runSF :: [a] -> [b] }
instance Arrow SF where
	arr f = SF (map f)
	(SF f) >>> (SF g) = SF (f >>> g) -- == SF (g . f)
	-- (&&&) :: ([a] -> [b]) -> ([a] -> [c]) -> [a] -> [(b, c)]
	(SF f) &&& (SF g) = SF $ f &&& g >>> uncurry zip
	-- (***) :: ([a] -> [b]) -> ([c] -> [d]) -> [(a, b)] -> [(c, d)]
	(SF f) *** (SF g) = SF (unzip >>> (f *** g) >>> uncurry zip) -- SF $ \x -> zip (f $ map fst x) (g $ map snd x) -- \x -> [(head(f [a]), head(g [c])) | (a, c) <- x]
	-- first :: ([a] -> [b]) -> [(a, c)] -> [(b, c)]
	first (SF f) =  SF (unzip >>> first f >>> uncurry zip) -- SF $ \x -> zip (f $ map fst x) (map snd x)

delay :: a -> SF a a
delay x = SF (x:)

-----

addA :: Arrow arr => arr a Int -> arr a Int -> arr a Int
addA f g = (f &&& g) >>> arr (uncurry (+))

-----

main :: IO ()
main = do 
	i <- runKleisli (count "m") "./arrow.hs"
	print i
	let a = runSF (arr (+1)) [1 .. 10::Int]
	print a
	print $ runSF (delay 0) a
	print $ runSF (arr (+ 1) &&& arr ((*5) . (+1))) [1 .. 10 :: Int]
	print $ runSF (arr id &&& delay 0) [1 .. 5 :: Int]
	print $ ((2*) `addA` (2+)) 5
	print $ runSF (arr id *** delay 0) [(a', b') | a' <- [1 .. 5::Int], b' <- [1 .. 2::Int]]
