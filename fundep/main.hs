{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
data Vector = Vector Int Int deriving (Eq, Show)
data Matrix = Matrix Vector Vector deriving (Eq, Show)

instance Num Vector where
	(Vector a b) + (Vector c d) = Vector (a+c) (b+d)
	(Vector a b) * (Vector c d) = Vector (a*c) (b*d)
	abs (Vector a b) = Vector (abs a) (abs b)
	signum _ = undefined
	fromInteger _ = undefined

class Mult a b c | a b -> c where
	(.*.) :: a -> b -> c


instance Mult Vector Vector Matrix where
	(Vector a b) .*. (Vector c d) = Matrix (Vector (a*c) (a*d)) (Vector (b*c) (b*d))

instance Mult Int Vector Vector where
	d .*. (Vector a b) = Vector (d*a) (d*b)


main :: IO ()
main = print "hello"