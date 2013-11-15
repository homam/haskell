import Data.Foldable (Foldable, foldMap)
import Data.Monoid
 
data Tree a = Leaf a
              | Node [Tree a] 
     deriving (Show) 
 
instance Functor (Tree) where
    fmap f (Leaf t) = Leaf (f t) 
    fmap f (Node t) = Node [fmap f a | a <- t] --Node (fmap (fmap f) t)


instance Foldable (Tree) where
	foldMap f (Leaf t) = f t
	foldMap f (Node (x:xs)) = foldMap f x `mappend` foldMap f (Node xs)
	foldMap _ (Node []) = mempty

    --foldMap f (Leaf t) = f t
    --foldMap f (Node (Leaf x : xs)) = foldMap f (Leaf x) `mappend` foldMap f (Node xs)
    --foldMap f (Node (Node x : xs)) = foldMap f (Node x) `mappend` foldMap f (Node xs)
    --foldMap _ (Node []) = mempty

instance (Monoid a) => Monoid (Tree a) where -- for mconcat [Tree]
	mempty = Node mempty
	mappend (Leaf a) (Leaf b) = Leaf (a `mappend` b)
	mappend (Node a) (Leaf b) = Node (Leaf b : a)
	mappend (Leaf a) (Node b) = Node (Leaf a : b)
	mappend (Node a) (Node b) = Node (a ++ b)


main :: IO ()
main = do
	let tree = Node [Leaf 'A', Leaf 'B', Node [Leaf 'E', Leaf 'F']]
	print $ fmap (:"*") tree
	print $ foldMap (:[]) tree -- all the leaves
	print $ foldMap (:"*") tree

	let tree2 = Node [Leaf "C", Leaf "D"]
	print $ mconcat [tree2, tree2]