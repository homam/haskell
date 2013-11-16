{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

import Data.Foldable (Foldable, foldMap)
import Data.Monoid


data TreeItem a b where
	TreeItem :: (HasDefaultValue a) => a -> a -> TreeItem a a

instance (Show a) => Show (TreeItem a a) where
	show (TreeItem a b) = "TreeItem " ++ show a ++ " " ++ show b


class (Eq a) => (HasDefaultValue a) where defaultValue :: a

instance HasDefaultValue Char where defaultValue = '-' 

instance HasDefaultValue String where defaultValue = "" 

instance HasDefaultValue Int where defaultValue = 0




--data Tree a = Leaf a
--              | Node a [Tree a] 
--     deriving (Show) 
 
--instance Functor (Tree) where
--    fmap f (Leaf t) = Leaf (f t) 
--    fmap f (Node t ts) = Node (f t) [fmap f a | a <- ts] --Node (fmap (fmap f) t)


--instance Foldable (Tree) where
--	foldMap f (Leaf t) = f t
--	foldMap f (Node t (x:xs)) = foldMap f x `mappend` foldMap f (Node t xs)
--	foldMap f (Node t []) = f t


--instance (Monoid a) => Monoid (Tree a) where -- for mconcat [Tree]
--	mempty = Node mempty []
--	mappend (Leaf a) (Leaf b) = Leaf (a `mappend` b)
--	mappend (Node a as) (Leaf b) = Node a (as ++ [Leaf b])
--	mappend (Leaf a) (Node b bs) = Node a (Leaf b : bs)
--	mappend (Node a as) (Node b bs) = Node a (as ++ [Leaf b] ++ bs)



data Tree a = Tree a [Tree a] deriving (Show)

-- not complete!
instance (Monoid a, HasDefaultValue a) => Monoid (Tree a) where -- for mconcat [Tree]
	mempty = Tree mempty []
	mappend ta@(Tree a []) tb@(Tree b [])
		| a == b = Tree a []
		| otherwise = Tree defaultValue [ta, tb]
	mappend (Tree a as) (Tree b [])
		| a == b = Tree a as
		| otherwise = Tree a[] --]mconcat (b:as))


addChild :: (HasDefaultValue a) => a -> a -> Tree a -> Tree a
addChild parent value original@(Tree r []) 
	| r == parent = Tree r [Tree value []]
	| otherwise = original
addChild parent value (Tree r (a:as))
	| r == parent = Tree r ((a:as) ++ [Tree value []])
	| otherwise = Tree r (addChild parent value a : map (addChild parent value) as)


addTree :: (HasDefaultValue a) => Tree a -> Tree a -> Tree a
addTree l@(Tree x' []) r@(Tree y' _)
	| x' == y' = r
	| otherwise = l
addTree (Tree x' (x:xs)) r@(Tree y' ys)
	| x' == y' = Tree x' ((x:xs) ++ ys)
	| otherwise = Tree x' ((x `addTree` r) : map (`addTree` r) xs)

--tiAppend :: (HasDefaultValue a) => Tree a -> TreeItem a -> Tree a
--tiAppend (Leaf a) (TreeItem parent value) = Leaf a

main :: IO ()
main = do 
	print "trees"
	--print $ TreeItem 1 1
	print $ TreeItem (1::Int) (2::Int)
	print $ TreeItem "A" "C"


	let tree = Tree '-' [Tree 'A' [], Tree 'B' [], Tree 'C' [Tree 'E' [], Tree 'F' []], Tree 'L' []]
	print $ addChild '-' 'X' tree

	let tree2 = Tree 'E' [Tree 'X' [], Tree 'Y' []]
	print $ tree `addTree` tree2	
	--print $ foldMap (:[]) tree
	--print $ foldMap (:"*") tree

	--let tree2 = Node "" [Leaf "A", Leaf "B", Node "C" [Leaf "E", Leaf "F"]]
	--let tree3 = Node "" [Leaf "C", Leaf "D"]
	--Ïrint $ mconcat [tree2, tree3]

	--let items = [TreeItem defaultValue "A", TreeItem "B" "A"]
	--print items