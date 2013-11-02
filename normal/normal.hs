module Normal (
	randomValues,
	normalTest,
	maybeTupleToConversionList,
	parse,
	conversionRate,
	mainTest
	) where

import System.IO
import Text.Read (readMaybe)
import System.Random hiding (split)


conversionRate :: [Bool] -> Double
--conversionRate ls = (length . filter (==True)) ls `devF` length ls
conversionRate ls = cRate ls 0 0 -- this is a faster implementation of conversionRate that scans list once
	where
		cRate [] v s = s / v
		cRate (x:xs) v s = cRate xs (v+1) (s+isS)
			where isS = if x then 1 else 0


-- randomization

-- Randomly selects subsets of n items from the given list [a] for m times
selectManyRandoms :: (RandomGen g, Num n, Eq n) => n -> [a] -> n -> g -> ([[a]], g)
selectManyRandoms m ls n gen = ([[ls!!i | i <- is] |  is <- indices ], gen')
	where
		(indices, gen') = manyFiniteRandoms m (0, lenLs) n gen
		lenLs = length ls -1

-- Randomly selects a subset of n items from the given list [a]
selectRandom :: (RandomGen g, Num n, Eq n) => [a] -> n -> g -> ([a], g)
selectRandom ls n gen = (map snd . filter ((`elem` indices) . fst) $ [0..] `zip` ls, gen')
	where
		(indices, gen') = finiteRandoms (0, lenLs) n gen
		lenLs = length ls -1

-- Genrates lists of n random numbers between and including (lo, hi) for m times
manyFiniteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> (a, a) -> n -> g -> ([[a]], g)
manyFiniteRandoms 0 (_, _) _ gen = ([], gen)
manyFiniteRandoms m (lo, hi) n gen = (s:rest, gen'')
	where 
		(rest, gen'') = manyFiniteRandoms (m-1) (lo, hi) n gen'
		(s, gen') = finiteRandoms (lo, hi) n gen

-- Genrates a list of n random numbers between and including (lo, hi)
finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => (a, a) -> n -> g -> ([a], g)  
finiteRandoms (_, _) 0 gen = ([], gen)  
finiteRandoms (lo, hi) n gen =   
    let (value, newGen) = randomR (lo,hi) gen  
        (restOfList, finalGen) = finiteRandoms (lo, hi) (n-1) newGen  
    in  (value:restOfList, finalGen)


-- Coin

-- | Generates lists of n random vaues of type a for m times
manyRandomValues :: (RandomGen g, Random a) => Int -> Int -> g -> ([[a]], g)
manyRandomValues 0 _ gen = ([], gen)
manyRandomValues m n gen = (c:rest, gen'')
	where
		(c, gen') = randomValues n gen
		(rest, gen'') = manyRandomValues (m-1) n gen'


-- | Generates a list of n random values of type a
randomValues :: (RandomGen g, Random a) => Int -> g -> ([a], g)
randomValues 0 gen = ([], gen)
randomValues n gen = (c:rest, gen'')
	where
		(c, gen') = random gen
		(rest, gen'') = randomValues (n-1) gen'



-- | Does a normal test for any [Bool]
normalTest :: (Eq n, Num n, RandomGen t) =>
     Double -> n -> n -> [Bool] -> t -> [(Double, Int)]
normalTest numberOfBins trials selectionSize ds gen = 
	let
		(selection, _) = selectManyRandoms trials ds selectionSize gen
		crates = map conversionRate selection
		(lo, hi) = extents crates
		bs = bins numberOfBins lo hi
	in map (\x -> (fst x, length $ snd x)) $ groupWithBins bs crates




-- | Puts the values of [ls] in the bins specified by [bs]
groupWithBins :: (Fractional a, Ord a) => [(a, a)] -> [a] -> [(a, [a])]
groupWithBins bs ls = map (\((lo, hi), isLast) -> (mid lo hi,filter (isIn lo hi isLast) ls)) bs'
	where 
		isIn l h False s = (s >= l) && (s < h)
		isIn l h True s = (s >= l) && (s <= h) -- if it's the last bin, then the maximum is inclusive
		mid l h = (l+h) / 2.0
		bs' = [(x,i==(length bs -1)) | (x,i) <- bs `zip` [0..]]

extents :: Ord a => [a] -> (a, a)
extents ls = (minimum ls, maximum ls)

bins :: (Enum t, Fractional t) => t -> t -> t -> [(t, t)]
bins n lo hi = [(size*i+lo,size*(i+1)+lo) | i <- is]
	where
		is = [0.0..(n-1)] 
		size = (hi - lo)/n




-- CSV

-- Parses the CSV file to [(VisitId, SubscriberId)]
parse :: String -> [(Maybe Int, Maybe Int)]
parse content = [(a,b) | [a,b] <- ws]
	where
		ws = [parseList $ split (==',') l | l <- ls]
		ls = take 10000 $ tail $ lines content

		-- Parses each line of the CSV file
		parseList :: [String] -> [Maybe Int]
		parseList = map (\s -> readMaybe s :: Maybe Int)


-- | Converts the result of parse to [Bool]
maybeTupleToConversionList :: [(Maybe t, Maybe t1)] -> [Bool]
maybeTupleToConversionList = map isJustBoth
	where 
		isJust m = case m of
			Just _ -> True
			Nothing -> False
		isJustBoth (a, b) = isJust a && isJust b


-- | Split the given String by the condition p
split :: (Char -> Bool) -> String -> [String]
split p s
	| s == "" = []
	| otherwise = m : split p (tail' s')
		where 
			(m, s') = break p s 
			tail' str
				| str == "" = ""
				| otherwise = tail str





mainTest :: IO ()
mainTest = do
	
	gen <- newStdGen
	let (coins, gen') = randomValues 10000 gen :: ([Bool], StdGen)
	print $ normalTest 2 500 100 coins gen'
	print $ conversionRate coins
	putStrLn "--"

	contents <- readFile "iraq.csv"
	let ds = maybeTupleToConversionList $ parse contents
	print $ normalTest 10 500 500 ds gen
	print $ conversionRate ds

	putStrLn "--"

