data Point = Point Int Int

instance Show Point where
	show (Point x y) = "(" ++ (show x) ++ ", " ++ (show y) ++ ")"

instance Eq Point where
	(Point x1 y1) == (Point x2 y2) = x1 == x2 && y1 == y2

instance Ord Point where
	(Point x1 y1) `compare` (Point x2 y2) = ...

data Range = Range Point Point

instance Show Range where
	show (Range p1 p2) = "[" ++ (show p1) ++ " - " ++ (show p2) ++ "]"
