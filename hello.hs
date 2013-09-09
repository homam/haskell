import Data.Char

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
			return ()

upper = foldr (\x acc -> x:['!']++acc) [] . map toUpper