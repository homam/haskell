{-# LANGUAGE DataKinds #-}


(|>) :: a -> (a -> b) -> b
x |> f = f x

type Birds = Int
data Pole = Pole Birds Birds deriving (Show)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft b (Pole left right) 
	| abs(left + b - right) > 4 = Nothing
	| otherwise = Just $ Pole (left + b) right

landRight :: Birds -> Pole -> Maybe Pole
landRight b (Pole left right)
	| abs(left - b + right) > 4 = Nothing
	| otherwise = Just $ Pole left (right + b)

playWithPole :: Pole -> Maybe Pole
playWithPole p = do
	p1 <- landLeft 3 p
	p2 <- landRight 2 p1
	Just p2

main :: IO ()
main = do
	print "Birds"
	print $ Pole 0 0 |> landLeft 2 >>= landRight 4
	-- this works perfectly but sublime complaints!
	--print $ return (Pole 0 0) >>= landLeft 2 >>= landRight 4

	print $ playWithPole $ Pole 0 0