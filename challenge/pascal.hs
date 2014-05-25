
fact :: (Eq a, Integral a) => a -> a
fact n = fact' n n
	where
	fact' 0 _ = 1
	fact' 1 _ = 1 
	fact' n' x = fact' (n'-1) (n'-1)*x

--n! / (r! * (n-r)!)
pascal :: Integral a => a -> a -> a
pascal n r = fact n `div` ( fact r * fact (n - r) )

row :: Int -> String
row n = unwords [show $ pascal n r | r <- [0 .. n]]

triangle :: Int -> String
triangle k = foldl1 (\a b -> a ++ "\n" ++ b) paddedRows
	where 
	rows = [row n | n <- [0 .. k-1]]
	lastRow = last rows
	lastRowLength = length lastRow
	repl n = foldl1 (++) $ replicate (n+1) " "
	paddedRows = map (\a -> repl ((lastRowLength - length a) `div` 2) ++ a) rows

main :: IO ()
main = do 
	inputdata <- getContents
	let k = read inputdata :: Int
	putStrLn $ triangle k
