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
			 Node Int NTree NTree

instance Show NTree where
	show = (showNTreeDepth 0) 

-- Node 12 (Node 14 NilT NilT) (Node 15 (Node 17 NilT (Node 18 (Node 19 NilT NilT) (Node 20 NilT NilT)))  NilT)
-- Node 12 (Node 14 NilT NilT) (Node 15 (Node 17 NilT (Node 18 (SNode 19) (SNode 20)))  NilT)

showNTreeLeaf :: Int -> NTree -> String
showNTreeLeaf depth NilT = ""
showNTreeLeaf depth leaf = "\n" ++ ((++ "|--") $ concat $ replicate (depth) "   ") ++ (showNTreeDepth (depth+1) leaf)

showNTreeDepth depth (NilT) = "Nill!!"
showNTreeDepth depth (Node value left right) =
	"|" ++ (show value) ++ (showNTreeLeaf depth left) ++ (showNTreeLeaf depth right)

