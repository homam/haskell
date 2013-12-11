{-# LANGUAGE MultiParamTypeClasses , FunctionalDependencies , FlexibleInstances  #-}

data Vector2 a = Vector2 a a

instance (Show a) => Show (Vector2 a) where
	show (Vector2 a b) = "[" ++ show a ++ "," ++ show b ++ "]"


class Mult a b c | a b -> c where
  (>*<) :: a -> b -> c


class Dot a b c | a b -> c where
  (>.<) :: a -> b -> c


instance (Num a) => Mult (Vector2 a) (Vector2 a) (Vector2 a) where
	(Vector2 a b) >*< (Vector2 a' b') = Vector2 (a*a') (b*b')


instance (Num a) => Dot (Vector2 a) (Vector2 a) a where
	(Vector2 a b) >.< (Vector2 a' b') = (a*a') + (b*b')




data Vector a = Vector [a]

instance (Show a) => Show (Vector a) where
	show (Vector [a]) = show [a]

instance (Num a) => Dot (Vector a) (Vector a) a where
	(Vector r) >.< (Vector s) = sum $ zipWith (*) r s


main :: IO ()
main = do
	let v2 = Vector2 (4.5 :: Double) 5.6
	let v2' = Vector2 (3 :: Double) 2
	print (v2 >*< v2')
	print (v2 >.< v2')

	let v = Vector ([3, 4, 5, 6] :: [Int])
	let v' = Vector ([2, 5, 0, 0] :: [Int])
	print (v >.< v')