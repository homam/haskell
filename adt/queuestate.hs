module QueueState (
	QueueState,
	addMessage, -- Inmess -> QueueState -> QueueState
	queueStep, -- QueueState -> (QueueState, [Outmess])
	queueSart, -- QueueState
	queueLength, -- QueueState -> Int
	queueEmpty -- QueueState -> Bool
	) where

data QueueState QS Time Service [Inmess]
	deriving (Eq, Show)

addMessage :: Inmess -> QueueState -> QueueState
addMessage im (QS time serv ims) -> QS time serv (ims++[im])

queueStep :: QueueState -> (QueueState, [Outmess])
queueStep (QS time servSoFar (Yes arr serv : inRest))
