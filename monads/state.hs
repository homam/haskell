import Debug.Trace

type Stack x = [x]

pop :: Stack x -> (x, Stack x)
pop (x:xs) = (x, xs)

push :: x -> Stack x -> ((), Stack x)
push x xs = ((), x:xs)



--playWithStack :: State (Stack Integer) Integer
playWithStack :: Stack Integer -> (Integer, [Integer])
playWithStack = do
	push (3::Integer)
	push (7::Integer)
	push (10::Integer)
	_ <- pop
	pop




-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
newtype State s a = State { runState :: s -> (a, s) }
instance Monad (State s) where
	return x = State $ \s -> (x,s)
	(State h) >>= f = State $ \s -> let
		(a, newState) = h s
		(State g) = f a  
		in g newState 
	--m >>= k  = State $ \s -> let
	--	(a, s') = runState m s
	--	in runState (k a) s'


pop' :: State (Stack x) x
pop' = State $ \(x:xs) -> (x, xs)

push' :: x -> State (Stack x) ()
push' x = State $ \xs -> ((), x:xs)


playWithStack' :: State (Stack Int) Int
playWithStack' = do
	push' 3
	push' 7
	push' 10
	a <- pop'
	push' $ trace (show a) a
	pop'


main :: IO ()
main = do
	let stack = [1::Integer,2,3]
	let (_, stack1) = pop stack
	let ((), stack2) = push 5 stack1
	print stack2
	print " ---- "
	print $ playWithStack [4,5]
	print " ---- ---- "
	let st = runState playWithStack' [4,5]
	print st