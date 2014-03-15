import Data.Ratio
import Control.Arrow (first, second)
import Control.Applicative
--import Debug.Trace

newtype Prob a = Prob { unProb :: [(a, Rational)] } deriving (Show)

instance Functor Prob where
	fmap f (Prob xs) = Prob $ map (first f) xs -- $ map (\(a, r) -> (f a, r)) xs

instance Monad Prob where
	return a = Prob [(a, 1 % 1)]
	--Prob [] >>= _ = Prob []
	--Prob (x:xs) >>= f = do
	--	x' <- f $ fst x
	--	Prob $ (x', snd x) : unProb (Prob xs >>= f)

	m >>= f = joinProb (fmap f m)


joinProb :: Prob (Prob a) -> Prob a
joinProb (Prob []) = Prob []
joinProb (Prob (x:xs)) = Prob $ map (second (r *)) xs' ++ unProb (joinProb $ Prob xs)
	--Prob $ map (\(x',r') -> (x', r*r')) xs' ++ unProb (joinProb $ Prob xs)
	where (Prob xs', r) = x

flatten :: Prob (Prob a) -> Prob a  
flatten (Prob xs) = Prob $ concatMap multAll xs  
    where multAll (Prob innerxs,p) = map (second (p *)) innerxs --map (\(x,r) -> (x,p*r)) innerxs 

thisSituation :: Prob (Prob Char)  
thisSituation = Prob  
    [( Prob [('a',1%2),('b',1%2)] , 1%4 )  
    ,( Prob [('c',1%2),('d',1%2)] , 3%4)   
    ]  


main :: IO ()
main = do 
	print "Hello Prob"
	print $ Prob [(3 :: Integer,1%2),(5,1%4),(9,1%4)]
	print $ show <$> Prob [(3 :: Integer,1%2),(5,1%4),(9,1%4)] -- = fmap show $ Prob ...
	print $ flatten thisSituation
	print $ joinProb thisSituation