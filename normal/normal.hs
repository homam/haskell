import System.IO
import Text.Read (readMaybe)
import System.Random hiding (split)
import Debug.Trace (trace)
import Data.List (minimum, maximum)
import GHC.Exts (groupWith)
--import Data.Maybe (listToMaybe)
--import Control.Monad

conversionRate :: (Fractional a) => [(Maybe Int, Maybe Int)] -> a
conversionRate ds = countJusts [b | (_,b) <- ds] `devF` countJusts [a | (a,_) <- ds]

countJusts :: [Maybe a] -> Int
countJusts = length . filter isJust
	where isJust m = case m of
		Just _ -> True
		Nothing -> False

devF :: (Fractional a, Integral b) => b -> b -> a
devF a b = fromIntegral a / fromIntegral b

-- Parses the CSV file
parse :: String -> [(Maybe Int, Maybe Int)]
parse content = [(a,b) | [a,b] <- ws]
	where
		ws = [parseList $ split (==',') l | l <- ls]
		ls = take 10000 $ tail $ lines content

		-- Parses each line of the CSV file
		parseList :: [String] -> [Maybe Int]
		parseList = map (\s -> readMaybe s :: Maybe Int)


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
		lenLs = (length ls) -1

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


main :: IO ()
main = do
	
	contents <- readFile "iraq.csv"
	let ds = parse contents
	--print $ 
	--print $ countJusts [a | (a,_) <- ds]
	--print $ countJusts [b | (_,b) <- ds]

	gen' <- newStdGen
	let (selection, _) = selectManyRandoms 500 ds 1000 gen'
	let crates = map conversionRate selection
	let (lo, hi) = extents crates
	print (lo, hi)
	let bs = bins 10 lo hi -- $ (fromIntegral bins)/10.0
	print $ map length $ groupWithBins bs crates

	--print $ manyFiniteRandoms (1,10) 2 gen
	print $ conversionRate ds

	putStrLn "--"



--

groupWithBins bs ls = map (\(lo, hi) -> filter (isIn lo hi) ls) bs
	where isIn l h s = (s >= l) && (s < h)

extents :: Ord a => [a] -> (a, a)
extents ls = (minimum ls, maximum ls)

bins :: (Enum t, Fractional t) => t -> t -> t -> [(t, t)]
bins n lo hi = [(size*i+lo,size*(i+1)+lo) | i <- is] --[(lo*i,(trace ("====" ++ (show $ (i*size)+lo)) ((lo*i)+size))) | i <- is]
	where
		is = [0.0..(n-1)] 
		size = (hi - lo)/n

-- utils

split :: (Char -> Bool) -> String -> [String]
split p s
	| s == "" = []
	| otherwise = m : split p (tail' s')
		where 
			(m, s') = break p s 
			tail' str
				| str == "" = ""
				| otherwise = tail str


--maybeRead :: Read a => String -> Maybe a
--maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads
