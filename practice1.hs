doubleme x = x + x
nsme' x n =  (if n>1 then (nsme' x (n-1)) else 0) + x

-- list comprehension
ls = [n | n <- [1..], n `mod` 3 == 0]
fls ys = [x*y | x <- [20*n | n <- ys], y <- ys]
--take 10 (fls [1..10])

-- tuples:
tup = [("NYC", 14), ("DC", 2), ("SEA", 5)]
-- zip function:
numberNames = zip [1..] ["one", "two", "three", "four", "five"]


rightTriangles = [ (a,b,c) | c <- [1..], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
--take 3 rightTriangles
rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]

-- define our own length function, using pattern matching and recursion
length':: (Num r) => [a] -> r
length' [] = 0
length' (_:xs) = 1 + length' xs

-- let binding
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- recursion with pattern matching in function arguments
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' ls@(x:xs)
    | x> tails   = x
    | otherwise = tails
    where tails = maximum' xs -- (tail ls)


maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

-- recursion with gaurds
replicate' :: (Num i, Ord i) => i -> a -> [a] -- note: Num is not a sub class of Ord
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x


-- take
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n<= 0     = []
take' _ []      = []
take' n (x:xs)  = x:take' (n-1) xs -- recursion continues till either n == 0 or xs == []

-- reverse
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

-- zip
zip' :: [a] -> [b] -> [(a,b)]
zip' _ []           = []
zip' [] _           = []
zip' (x:xs) (y:ys)  = (x,y):zip' xs ys

-- elem
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ []     = False
elem' y (x:xs) = (if x == y then True else elem' y xs)


-- quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort []        = []
quicksort (x:xs)    = quicksort [y | y <- xs, y < x] ++ [x] ++ quicksort [y | y <- xs, y >= x]

quicksort' :: (Ord a) => [a] -> [a]
quicksort' []       = []
quicksort' (x:xs)   = let   left    = [y | y <- xs, y <x]
                            right   = [y | y <- xs, y >= x]
                      in    quicksort' left ++ [x] ++ quicksort' right



-- higher order functions

