data People = Person Name Age
--	deriving (Show)
type Name = String
type Age = Int


showPerson :: People -> String
showPerson (Person name age) = name ++ ", " ++ show age

-- implement my own Show instance for People type
instance Show People where
	show = showPerson
	--show (Person name age) = name ++ ", " ++ show age



data Shape = Circle Float |
			 Rectangle Float Float
			 deriving (Show)

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h



-- recursive definitions
-- let's parse an expression

data Expr = Lit Int |
			Add Expr Expr |
			Sub Expr Expr

instance Show Expr where
	show = showExpr

--  ((Lit 2) `Add` (Lit 5)) `Sub` ((Lit 5) `Add` (Lit 12))

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add (Lit n1) (Lit n2)) = (show n1) ++ " + " ++ (show n2) -- no paranthesis around expressions that are just Lit
showExpr (Add e1 e2) = "(" ++ (showExpr e1) ++ ") + (" ++ (showExpr e2) ++ ")"
showExpr (Sub e1 e2) = "(" ++ (showExpr e1) ++ ") - (" ++ (showExpr e2) ++ ")"


-- trees

data NTree = NilT |
			 NNode Int NTree NTree

instance Show NTree where
	show = (showNTreeDepth 0) 

-- NNode 12 (NNode 14 NilT NilT) (NNode 15 (NNode 17 NilT (NNode 18 (NNode 19 NilT NilT) (NNode 20 NilT NilT)))  NilT)
-- NNode 12 (NNode 14 NilT NilT) (NNode 15 (NNode 17 NilT (NNode 18 (SNNode 19) (SNNode 20)))  NilT)

showNTreeLeaf :: Int -> NTree -> String
showNTreeLeaf depth NilT = ""
showNTreeLeaf depth leaf = "\n" ++ ((++ "|--") $ concat $ replicate (depth) "   ") ++ (showNTreeDepth (depth+1) leaf)

showNTreeDepth depth (NilT) = "Nill!!"
showNTreeDepth depth (NNode value left right) =
	"|" ++ (show value) ++ (showNTreeLeaf depth left) ++ (showNTreeLeaf depth right)


-- polymorphuc tree

data Tree a = Nil |
			  Node a (Tree a) (Tree a)
			  deriving (Eq, Ord, Show, Read)

depth :: Tree a -> Int
depth Nil = 0
depth (Node n left right) = 1 + max (depth left) (depth right)

collapse :: Tree a -> [a]
collapse Nil = []
collapse (Node n left right) = (collapse left) ++ [n] ++ (collapse right)

mapTree :: (a -> b) -> Tree a -> Tree b --  (mapTree (\a -> 2*a)) $ Node 2 (Node 4 Nil Nil) Nil
mapTree f Nil = Nil
mapTree f (Node n left right) = Node (f n) (mapTree f left) (mapTree f right)


----


data MyMaybe a = MyNothing | MyJust a
			   deriving (Show, Read, Eq, Ord)

mydiv :: Int -> Int -> MyMaybe Int
mydiv x y
	| (y /= 0)	= MyJust (x `div` y)
	| otherwise	= MyNothing

mapMyMaybe :: (a -> b) -> MyMaybe a -> MyMaybe b
mapMyMaybe f MyNothing	= MyNothing
mapMyMaybe f (MyJust x)	=  MyJust (f x)

myMaybe ::  b -> (a -> b) -> MyMaybe a -> b
myMaybe x f MyNothing = x
myMaybe x f (MyJust y) = (f y)

-- myMaybe 0 id (mapMyMaybe (2*) (MyJust 12))
-- myMaybe 0 id $ mapMyMaybe (2*) (12 `mydiv` 0)