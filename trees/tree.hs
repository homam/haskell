data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Read, Eq)

instance Show a => Show (Tree a) where
	show = showWithDepth ""
	
showWithDepth :: Show a => String -> Tree a -> String
showWithDepth _ Empty = ""
showWithDepth padding (Node a left right) = 
	"\n" ++ padding ++ "--" ++ show a  ++ showWithDepth (padding ++ "  | ") left ++ showWithDepth (padding ++ "    ") right


leaf :: a -> Tree a
leaf a = Node a Empty Empty

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

cutTree :: Directions -> Tree a -> Tree a
cutTree (L:dirs) (Node a left right) = Node a (cutTree dirs left) right
cutTree (R:dirs) (Node a left right) = Node a left (cutTree dirs right)
cutTree [] _ = Empty

(+>) :: Tree a -> Tree a -> Tree a
(+>) (Node a left Empty) = Node a left

(<+) :: Tree a -> Tree a -> Tree a
(<+) (Node a Empty right) new = Node a new right

(</) :: Tree a -> Tree a -> Tree a
(</) left (Node a Empty right) = Node a left right

(/>) :: Tree a -> Tree a -> Tree a
(/>) right (Node a left Empty) = Node a left right

freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (leaf 'N')  
                (leaf 'J' +> leaf 'Q'))
            (Node 'Y'  
                (leaf 'S' <+ leaf 'B')  
                (leaf 'A')  
            ) 
        )  
        (Node 'V'  
            (Node 'W'  
                (leaf 'G')  
                (leaf 'Y')  
            )  
            (leaf 'L' </ leaf 'c' /> leaf 'R') 
        ) 

main :: IO ()
main = do
	print freeTree
	print $ replaceNode 'X' [L,L] freeTree
	print $ subTree [L,L] freeTree
	print $ cutTree [R,L] freeTree
