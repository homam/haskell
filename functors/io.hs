import Control.Applicative

main :: IO ()
main = do 
	line <- fmap reverse getLine
	--let line' = reverse line
	putStrLn line



data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)