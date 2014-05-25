-- f :: [x] -> [x]
f = map abs

-- This section handles the Input/Output and can be used as it is. Do not modify it.
main :: IO ()
main = do
   inputdata <- getContents
   mapM_ print (f $ map (read :: String -> Int) $ lines inputdata)