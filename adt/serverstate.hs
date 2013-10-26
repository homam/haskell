module ServerState(
	ServerState,
	addToQueue, -- Int -> Inmess -> ServerState
	serverStep, -- ServerState -> (ServerState, [Outmess])
	simulationStep, -- ServerState -> Inmess -> (ServerState, [Outmess])

	serverStart, -- ServerState
	serverSize, -- ServerState -> Int
	shortestQueue -- ServerState -> Int
	) where