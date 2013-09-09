import Data.Char
import Control.Monad

main = do 
	putStrLn "hello, name please?"
	name <- getLine
	when (not $ null name) $ do
		let bigName = upper name
		putStrLn ("Hey " ++ bigName)
		main
		
	putStrLn "goodbye!"
		

upper = foldr (\x acc -> x:['!']++acc) [] . map toUpper