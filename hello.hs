import Data.Char
import System.Process

main = do 
	putStrLn "hello, name please?"
	name <- getLine
	if not $ null name
		then do
			let bigName = upper name
 			putStrLn ("Hey " ++ bigName)
 			main
 		else do
			putStrLn "goodbye!"
			a <- readProcess "cat" ["hello.hs"] []
			print a
			a <- readProcess "ls" [] []
			let files = lines a
			print files
			
			return ()

upper = foldr (\x acc -> x:['!']++acc) [] . map toUpper

