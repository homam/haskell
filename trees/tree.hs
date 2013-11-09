data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Read, Eq)

instance Show a => Show (Tree a) where
	show = showWithDepth ""
	
showWithDepth :: Show a => String -> Tree a -> String
showWithDepth _ Empty = ""
showWithDepth padding (Node a left right) = 
	"\n" ++ padding ++ "--" ++ show a  ++ showWithDepth (padding ++ "  | ") left ++ showWithDepth (padding ++ "    ") right


data Direction = L | R deriving (Show, Eq, Read)
type Directions = [Direction]

replaceNode :: a -> Directions -> Tree a -> Tree a
replaceNode new (R:dirs) (Node a left right) = Node a left (replaceNode new dirs right)
replaceNode new (L:dirs) (Node a left right) = Node a (replaceNode new dirs left) right
replaceNode new [] (Node _ left right) = Node new left right

subTree :: Directions -> Tree a -> Tree a
subTree (L:dirs) (Node _ left _) = subTree dirs left
subTree (R:dirs) (Node _ _ right) = subTree dirs right
subTree [] tree = tree


freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        ) 

main :: IO ()
main = do
	print freeTree
	print $ replaceNode 'X' [L,L] freeTree
	print $ subTree [L,L] freeTree
