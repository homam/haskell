module Queue (
	Queue,
	isEmpty
	) where

newtype Queue a = Queue [a]

emptyQueue = Queue []

isEmpty :: Queue a -> Bool
isEmpty (Queue []) = True
isEmpty _ = False

enqueue :: Queue a -> a -> Queue a
enqueue (Queue xs) x = Queue (xs++[x])

dequeue :: Queue a -> (a, Queue a)
dequeue queue@(Queue xs)
	| not (isEmpty queue) = (head xs, Queue $ tail xs)
	| otherwise = error "dequeue: queue is empty!"