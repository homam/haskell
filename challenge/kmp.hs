import System.Environment 

properPrefixes, properSuffixes :: String -> Int -> [String]
properPrefixes _ 0 = [""]
properPrefixes str i = reverse [take j str' | j <- [1 .. length str']]
	where 
		str' = take i str

properSuffixes _ 0 = [""]
properSuffixes str i = [drop j str' | j <- [1 .. length str' - 1]]
	where
		str' = take (i+1) str

matchingTableValue :: String -> Int -> String
matchingTableValue str i =  head' $ filter (`elem` suffs) prefs 
	where
		prefs = properPrefixes str i
		suffs = properSuffixes str i
		head' [] = ""
		head' x = head x

matchingTable :: String -> [Int]
matchingTable str = -1 : [length $ matchingTableValue str i | i <- [0 .. length str - 2]]


main :: IO ()
main = do
	[s, w] <- getArgs
	print $ matchingTable w