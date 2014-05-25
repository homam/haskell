
data Range = Range Int Int deriving (Eq)
instance Show Range where
	show (Range a b) = "[" ++ show a ++ ", " ++ show b ++ "]"

data Tree a b = Tree {trange :: Range, tval :: b, tleft :: Tree a b, tright :: Tree a b} | Node Int a | Empty deriving (Eq)



instance (Show a, Show b) => Show (Tree a b) where
	show = unlines . treeIndent where
		treeIndent :: (Show a, Show b) => Tree a b -> [String]
		treeIndent (Node rng m) = ["-- /- " ++ show rng ++ "  ( " ++ show m ++ " )"]
		treeIndent (Tree rng m lb rb) =
			["--" ++ show rng ++ "  ( " ++ show m ++ " )"] ++
			map ("  |" ++) ls ++
			("  `" ++ r) : map ("   " ++) rs 
			where
		        (r:rs) = treeIndent rb
		        ls     = treeIndent lb



makeTree :: (Ord a) => Range -> [a] -> Tree a a
makeTree _ [] = Empty
makeTree (Range a _) (x:[]) = Node a x
makeTree (Range a b) (x:y:[]) = Tree (Range a b) (min x y) (Node a x) (Node b y)
makeTree r@(Range a b) xs = Tree r (min (getMin leftTree) (getMin rightTree)) leftTree rightTree where
	l = length xs
	lLeft = (l `div` 2) + (l `mod` 2)
	leftTree = makeTree (Range a (a+lLeft-1)) $ take lLeft xs
	rightTree = makeTree (Range (a+lLeft) b) $ drop lLeft xs

	getMin :: (Ord a) => Tree a a -> a
	getMin (Node _ a') = a'
	getMin (Tree _ b' _ _) = b'



findMin :: Range -> Tree Int Int -> Int
findMin (Range a b) (Node i m) = if i >= a && i <= b then m else maxBound :: Int
findMin r@(Range x y) (Tree (Range a b) m left right) 
	| a >= x && b <= y  = m
	| (x < a && y < a) || (x > b && y > b) = maxBound :: Int
	| otherwise = min left' right' where
		left'  = findMin r left
		right' = findMin r right



stringToInts :: String -> [Int]
stringToInts = map (read :: String -> Int) . words 

range :: Int -> Int -> [a] -> [a]
range x y = take (y-x+1) . drop x 

main :: IO ()
main = do
	inputdata <- getContents
	let (line1:lineData:queriesData) = lines inputdata
	let (_:_:[]) = stringToInts line1
	let arr = stringToInts lineData
	let tree = makeTree (Range 0 (length arr - 1)) arr
	let queries = map stringToInts queriesData
	mapM_ (print . (\(x:y:[]) -> findMin (Range x y) tree)) queries