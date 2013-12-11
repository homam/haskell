module Store (
	Store,
	initial, -- Store
	value, -- Store -> Int -> String
	update -- Store -> Int -> String -> Store
	) where

newtype Store = Sto (Int -> String)

initial :: Store
initial = Sto (\v -> "-")

value :: Store -> Int -> String
value (Sto sto) v = sto v

update :: Store -> Int -> String -> Store
update (Sto sto) v n = Sto (\w -> if v==w then n else sto w)

--(\x -> value x 0) $ (\x -> update x 2 "How") $ (\x -> update x 1 "hello") $ initial


newtype Matrix a = Matrix (Int -> Int -> a) 

mvalue :: Matrix a -> Int -> Int -> a
mvalue (Matrix m) = m




main = do
	putStrLn $ (\x -> value x 0) $ (\x -> update x 2 "How") $ (\x -> update x 1 "hello") $ initial	

	let m = Matrix (\1 1 -> 5 :: Int)
	print (mvalue m 1 1)
