data UpTriangle = UpTriangle Integer Integer Integer Integer

instance Show UpTriangle where
	show (UpTriangle left top width height) = show width ++ "x" ++ show height ++ " @ " ++ show left ++ "x" ++ show top

screenCols, screenRows :: Integer
screenCols = 2*63 + 1
screenRows = 2*32

contains :: Integer -> Integer -> UpTriangle -> Bool
contains r c (UpTriangle left top width height) = 
	--(abs(height-c)<r) && 
	(abs(height-c')<r')
	&& (r >= top) && (r <= height+top) && (c >= left) && (c <= left + width)
	where
		c' = c - left
		r' = r - top

printUpTriangle :: [UpTriangle] -> String
printUpTriangle ts = foldl1 (\a b -> a ++ "\n" ++ b) [printRow i | i <- [1 .. screenRows]]
	where
		printRow i = foldl1 (++) [if any (contains i j) ts then "1" else " " | j <- [1 .. screenCols]]

cut :: UpTriangle -> [UpTriangle]
cut (UpTriangle left top width height) = [
		UpTriangle (left + (width `div` 4)+1) top width' height',
		UpTriangle left (top + height `div` 2) width' height',
		UpTriangle (left + width'+1) (top + height `div` 2) width' height'
	]
	where 
		width' = width `div` 2
		height' = height `div` 2


cutN :: Int -> [UpTriangle]
cutN 0 = [UpTriangle 0 0 screenCols screenRows]
cutN i = concatMap cut $ cutN (i-1)

main :: IO ()
main = do
	inputdata <- getContents
	let k = read inputdata :: Int
	putStrLn $ printUpTriangle $ cutN k
