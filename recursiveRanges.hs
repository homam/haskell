-- data V a = S a | M [V a] deriving Show
data Range a = Single a a | Poly [Range a] deriving (Read)

range :: Ord a => a -> a -> Range a
range x y = Single (min x y) (max x y)

instance Show a => Show (Range a) where
	show (Single x1 x2) = "(" ++ (show x1) ++ ", " ++ (show x2) ++ ")"
	--show (Poly (r:rs)) = (show r) ++ (show rs)
	show (Poly r) = foldl (\acc a -> acc ++ (show a)) "" r

instance Eq a => Eq (Range a) where
	(Single x1 x2) == (Single y1 y2) = x1 == y1 && x2 == y2
	(Poly r1) == (Poly r2)
		| length r1 /= length r2 = False
		| otherwise = all id [p1 == p2 | (p1, p2) <- zip r1 r2]
	--Poly [Single 2 5] == Poly [Single 2 5, Poly [Single 6 8]]

instance Ord a => Ord (Range a) where
	(Single x1 x2) `compare` (Single y1 y2) = x1 `compare` y1
	(Poly (x:xs)) `compare` (Poly (y:ys)) = x `compare` y


lenR :: Num a => Range a -> a
lenR (Single x1 x2) = x2 - x1
lenR (Poly r) = foldl (\acc a -> acc + (lenR a)) 0 r


intersection :: (Num a, Ord a) => Range a -> Range a -> Range a
intersection r1 r2 = _intersection (min r1 r2) (max r1 r2)
	where _intersection (Single x1 x2) (Single y1 y2)
		| x2 < y1 = Single 0 0
		| otherwise = Single (min y1 x2) (max y1 x2)


union :: (Num a, Ord a) => Range a -> Range a -> Range a
union (Single x1 x2) (Single y1 y2)
	| ((Single x1 x2) `intersection` r1) == (Single 0 0) = Single 1 1 --unionNoIntersection rx ry
	| otherwise = Single 0 0 
	--where
	--	unionNoIntersection (Single x1 x2) (Single y1 y2) = Single (min x1 y1) (max x2 y2)


--intersectionFraction :: (Fractional a, Ord a) => Range a -> Range a -> a
--intersectionFraction r1 r2 = (lenR $  intersection r1 r2) / (lenR $ union r1 r2)