screenCols, screenRows, firstYHeight :: Int
screenCols = 100
screenRows = 63
firstYHeight = 32

data Y = Y Int Int Int

contains :: Int -> Int -> Y -> Bool
contains c r (Y left top height) = onBottomLine || onLeftBranch || onRightBranch
	where
		onBottomLine = (r' <= 0) && (r <= top + height) && (c == c')
		onLeftBranch = 
			(r >= top) &&
			(r' > 0) && (c == c' - r') 

		onRightBranch = 
			(r >= top) &&
			(r' > 0) && (c == c' + r') 

		r' = top + (height `div` 2) - r
		c' = left + height `div` 2




printYs :: [Y] -> String
printYs ys = foldl1 (\a b -> a ++ "\n" ++ b) [printRow r | r <- [1 .. screenRows]]
	where
		printRow r = foldl1 (++) [if any (contains c r) ys then "1" else "_" | c <- [1 .. screenCols]]


branch :: Y -> [Y]
branch y@(Y left top height) = y :
      [Y (left - (height `div` 4)) (top `div` 2) (height `div` 2),
       Y (left + 3 * (height `div` 4)) (top `div` 2) (height `div` 2)]


cutN :: Int -> [Y]
cutN 1 = [Y ((screenCols-firstYHeight) `div` 2) firstYHeight firstYHeight]
cutN i = concatMap branch $ cutN (i-1)

main :: IO ()
main = do
	inputdata <- getContents
	let k = read inputdata :: Int
	putStrLn $ printYs $ cutN k