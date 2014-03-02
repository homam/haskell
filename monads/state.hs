type Stack x = [x]

pop :: Stack x -> (x, Stack x)
pop (x:xs) = (x, xs)

push :: x -> Stack x -> ((), Stack x)
push x xs = ((), x:xs)



-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
newtype State s a = State { runState :: s -> (a, s) }
instance Monad (State s) where
	return x = State $ \s -> (x,s)
	m >>= k  = State $ \s -> let
		(a, s') = runState m s
		in runState (k a) s'


--playWithStack :: State (Stack Integer) Integer
playWithStack :: Stack Integer -> (Integer, [Integer])
playWithStack = do
	push (3::Integer)
	push (7::Integer)
	push (10::Integer)
	a <- pop
	pop

main :: IO ()
main = do
	let stack = [1::Integer,2,3]
	let (_, stack1) = pop stack
	let ((), stack2) = push 5 stack1
	print stack2
	print " ---- "
	print $ playWithStack [4,5]