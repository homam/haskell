data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Read, Eq)

-- declare BinTree a to be an instance of Show
instance (Show a) => Show (Tree a) where
  -- will start by a '<' before the root
  -- and put a : a begining of line
  show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
    where
    -- treeshow pref Tree
    --   shows a tree and starts each line with pref
    -- We don't display the Empty tree
    treeshow _ Empty = ""
    -- Leaf
    treeshow pref (Node x Empty Empty) = pshow pref x

    -- Right branch is empty
    treeshow pref (Node x left Empty) =
                  pshow pref x ++ "\n" ++
                  showSon pref "`--" "   " left

    -- Left branch is empty
    treeshow pref (Node x Empty right) =
                  pshow pref x ++ "\n" ++
                  showSon pref "`--" "   " right

    -- Tree with left and right children non empty
    treeshow pref (Node x left right) =
                  pshow pref x ++ "\n" ++
                  showSon pref "|--" "|  " left ++ "\n" ++
                  showSon pref "`--" "   " right

    -- shows a tree using some prefixes to make it nice
    showSon pref before next t' =
                  pref ++ before ++ treeshow (pref ++ next) t'

    -- pshow replaces "\n" by "\n"++pref
    pshow pref x = replace '\n' ('\n':pref) (show x)

    -- replaces one char by another string
    replace c new =
      concatMap (change c new)
      where
          change c' new' x
              | x == c' = new'
              | otherwise = [x]

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
(</) (Node a Empty right) left = Node a left right

(/>) :: Tree a -> Tree a -> Tree a
(/>) (Node a left Empty) = Node a left -- eta reduced of (/>) (Node a left Empty) right = Node a left right

infixr 5 </
infixr 4 />


freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (leaf 'O'  
            </ (leaf 'L'  
                </ leaf 'N'
                /> (leaf 'J' /> leaf 'Q'))
            /> (leaf 'Y'  
                </ (leaf 'S' </ leaf 'B')  
                /> leaf 'A'
            ) 
        ) 
        (leaf 'V'
            </ (leaf 'W'
                </ leaf 'G'
                /> leaf 'Y')
            /> (leaf 'c'
                </ leaf 'L'
                /> leaf 'R')
        )
        

        --(
        --    (leaf 'G' </ leaf 'W' /> leaf 'Y') 
        --    </ leaf 'V' />
        --    (leaf 'L' </ leaf 'c'  /> leaf 'R') 
        --) 

main :: IO ()
main = do
    print $ replaceNode 'X' [L,L] freeTree
    print $ subTree [L,L] freeTree
    print $ cutTree [R,L] freeTree
    print freeTree
