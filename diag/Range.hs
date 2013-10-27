module Range (
	Range(..),
	range,
	lenR,
	unionR,
	intersectionR,
	intersectionFraction,
	normalizeRs,
	normalizeRList,
	prob
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

transformR :: Num a => a -> Range a -> Range a
transformR n (Range x1 x2) = Range (x1 + n) (x2 + n)

normalizeRs :: (Fractional a, Ord a) => (Range a, Range a) -> (Range a, Range a)
normalizeRs (x@(Range x1 x2), y@(Range y1 y2)) = (rnorm x, rnorm y)
	where 
		rnorm (Range w z) = Range (norm w) (norm z)
		norm v = (v - m) / l
		m = ((maximumR [x, y]) + (minimumR [x, y])) / 2
		l = ((maximumR [x, y]) - (minimumR [x, y])) / 2


normalizeRList :: (Fractional a, Ord a) => [Range a] -> [Range a]
normalizeRList ranges = map rnorm ranges
	where 
		rnorm (Range w z) = Range (norm w) (norm z)
		norm v = (v - m) / l
		m = ((maximumR ranges) + (minimumR ranges)) / 2
		l = ((maximumR ranges) - (minimumR ranges)) / 2


intersectionR :: (Num a, Ord a) => Range a -> Range a -> Range a
intersectionR r1 r2 = _intersectionR (min r1 r2) (max r1 r2)
	where _intersectionR (Range x1 x2) (Range y1 y2)
		| x2 < y1 = Range 0 0
		| otherwise = Range (min y1 x2) (max y1 x2)

unionR :: (Ord a, Num a) => Range a -> Range a -> [Range a]
unionR rx ry
	| intersectionR rx ry == Range 0 0 =  [rx, ry]
	| otherwise = [Range (minimumR [rx, ry]) (maximumR [rx, ry])]



minimumR :: (Ord a) => [Range a] -> a
minimumR rs = foldl1 (min) $ [min x1 x2 | (Range x1 x2) <- rs]

maximumR :: (Ord a) => [Range a] -> a
maximumR rs = foldl1 (max) $ [max x1 x2 | (Range x1 x2) <- rs]

intersectionFraction :: (Fractional a, Ord a) => Range a -> Range a -> a
intersectionFraction r1 r2 = (lenR $  intersectionR r1 r2) / (foldl (+) 0 $ map lenR $ unionR r1 r2)


-- | 95 % confidence interval
prob :: (Floating a, Ord a) => a -> a -> Range a
prob p n = range (p - c) (p + c)
	where
		c = 1.96 * sqrt(p * (1-p)/n) / 2