import Data.Monoid
import Control.Monad.Writer

newtype DiffList a = DiffList ([a] -> [a]) -- { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

appendToDiffList :: DiffList a -> a -> DiffList a
appendToDiffList (DiffList f) a = toDiffList $ f [a]


instance Monoid (DiffList a) where
	mempty = DiffList (\xs -> [] ++ xs)
	(DiffList f) `mappend` (DiffList g) = DiffList (f . g) -- (\xs -> f (g xs))

instance (Show a) => Show (DiffList a) where
	show = show . fromDiffList


-- performance test
-- 100000 must be the last number in the log

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = tell $ toDiffList ["0"]
finalCountDown x = do
	finalCountDown (x-1)
	tell $ toDiffList [show x]

finalCountDown' :: Int -> Writer [String] ()
finalCountDown' 0 = tell ["0"]
finalCountDown' x = do
	finalCountDown' (x-1)
	tell [show x]


main :: IO ()
main = do
	let a = toDiffList [1::Integer,2,3,4]
	let (DiffList af) = a
	let b = af [5]
	print b
	let c = appendToDiffList a 6
	print c
	print " ---- "
	let e = toDiffList [5,6,7,8]
	print $ a `mappend` e
	print " --- "
	--print $ runWriter $ finalCountDown 100000
	mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 100000
	print " --- "
	mapM_ putStrLn . snd . runWriter $ finalCountDown' 100000