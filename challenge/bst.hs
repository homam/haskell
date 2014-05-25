
data Tree = Empty | Node Int Tree Tree
instance Show Tree where
	show Empty = ""
	show (Node n left right) = show n ++ ", " ++ show left ++ ", " ++ show right

aTree = Node 3 (Node 2 (Node 1 Empty Empty) Empty) (Node 5 (Node 4 Empty Empty) (Node 6 Empty Empty))
main :: IO ()
main = print aTree