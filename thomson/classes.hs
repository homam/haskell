class Movable a where
	move :: Vector -> a -> a
	reflectX :: a -> a
	reflectY :: a -> a
	rotate180 :: a -> a
	rotate180 = reflectY . reflectX

data Vector = Vec Float Float

data Point = Point Float Float
	deriving Show

instance Movable Point where
	move = (Vec v1 v2) (Point p1 p2) = Point (p1+v1) (p2+v2)
	reflectX (Point p1 p2) = Point p1 (-p2)
	reflectY (Point p1 p2) = Point (-p1) p2
	-- rotate180 (Point p1 p2) = Point (-p1) (-p2) overrding rotate180


data Figure = Line Point Point |
			  Circle Point Float
			  deriving Show

instance Movable Figure where
	move vect (Line point1 point2) = Line (move vect point1) (move vect point2)
	move vect (Circle center radius) = Circle (move vect center) radius

	reflectX (Line point1 point2) = Line (reflectX point1) (reflectX point2)
	reflectX (Circle center radius) = Circle (reflectX center) radius

	reflectY (Line point1 point2) = Line (reflectY point1) (reflectY point2)
	reflectY (Circle center radius) = Circle (reflectY center) radius


instance  Movable a => Movable [a] where
	move v = map (move v)
	reflectX = map reflectX
	reflectY = map reflectY