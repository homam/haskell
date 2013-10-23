data Range = Range Int Int

range :: Int -> Int -> Range
range x y = Range (min x y) (max x y)

instance Show Range where
	show (Range x1 x2) = "(" ++ (show x1) ++ ", " ++ (show x2) ++ ")"


instance Eq Range where
	(Range x1 x2) == (Range y1 y2) = x1 == y1 && x2 == y2

instance Ord Range where
	(Range x1 x2) `compare` (Range y1 y2) = x1 `compare` y1


lenR :: Range -> Int
lenR (Range x1 x2) = x2 - x1


--intersection :: Range -> Range -> Int
--intersection r1 r2 = _intersection (min r1 r2) (max r1 r2)
--	where _intersection (Range x1 x2) (Range y1 y2) = (y1 - x2)/(y2 - x1)



intersection :: Range -> Range -> Range

intersection r1 r2 = _intersection (min r1 r2) (max r1 r2)
	where _intersection (Range x1 x2) (Range y1 y2)
		| x2 < y1 = Range 0 0
		| otherwise = Range (min y1 x2) (max y1 x2)


union :: Range -> Range -> Range
union (Range x1 x2) (Range y1 y2) = Range (min x1 y1) (max x2 y2)


intersectionFraction :: Fractional a => Range -> Range -> a
intersectionFraction r1 r2 = ((fromIntegral . lenR) $  intersection r1 r2) / ((fromIntegral . lenR) $  union r1 r2)