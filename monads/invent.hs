square :: Int -> Int
square x = x * x

double :: Int -> Int
double x = x + x

debug :: (Int -> Int) -> Int -> (Int, String)
debug f x = (f x, " -> " ++ show (f x))

square' :: Int -> (Int, String)
square' = debug square

double' :: Int -> (Int, String)
double' = debug double


dunit :: Int -> (Int, String)
dunit x = (x, show x)

dbind :: (Int -> (Int, String)) -> (Int, String) -> (Int, String)
dbind f (x, s) = (x', s'') where
	(x', s') = f x
	s'' = s ++ s'


dlift :: (Int -> Int) -> Int -> (Int, String)
dlift f = dunit . f -- dlift f x = (f x, show (f x))


(|*|) :: (Int -> (Int, String)) -> (a -> (Int, String)) -> a -> (Int, String)
f |*| g = dbind f . g

main :: IO ()
main = do
	print $ dbind double' . dbind square' $ dunit 5
	print $ (dbind square' . double') 6
	print $ dlift (\x -> x * x * x) 5
	print $ double' |*| square' |*| double' $ 5