module Range (
	Range,
	lenR,
	union,
	intersection,
	intersectionFraction
	) where

data Range a = Range a a deriving (Read)

range :: Ord a => a -> a -> Range a
range x y = Range (min x y) (max x y)

instance Show a => Show (Range a) where
	show (Range x1 x2) = "(" ++ (show x1) ++ ", " ++ (show x2) ++ ")"

instance Eq a => Eq (Range a) where
	(Range x1 x2) == (Range y1 y2) = x1 == y1 && x2 == y2

instance Ord a => Ord (Range a) where
	(Range x1 x2) `compare` (Range y1 y2) = x1 `compare` y1


lenR :: Num a => Range a -> a
lenR (Range x1 x2) = x2 - x1


intersection :: (Num a, Ord a) => Range a -> Range a -> Range a
intersection r1 r2 = _intersection (min r1 r2) (max r1 r2)
	where _intersection (Range x1 x2) (Range y1 y2)
		| x2 < y1 = Range 0 0
		| otherwise = Range (min y1 x2) (max y1 x2)


union :: (Ord a) => Range a -> Range a -> Range a
union (Range x1 x2) (Range y1 y2) = Range (min x1 y1) (max x2 y2)


intersectionFraction :: (Fractional a, Ord a) => Range a -> Range a -> a
intersectionFraction r1 r2 = (lenR $  intersection r1 r2) / (lenR $ union r1 r2)