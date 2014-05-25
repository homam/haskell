import Debug.Trace

data KMP a = KMP { done :: Bool , next :: a -> Int -> KMP a, index :: Int}
next' :: (Show a) => String -> KMP a -> a -> Int -> KMP a
next' msg k x i = trace ("next " ++ msg ++ " > " ++ show x ++ " >> " ++ show(index k + i)) $ next k x (index k + i)



isSubstringOf2 :: (Eq a, Show a) => [a] -> [a] -> Bool
isSubstringOf2 as bs = trace (show as ++ " isSubstringOf2 " ++ show bs) match (makeKmp as) bs
	where  
		match :: Show a => KMP a -> [a] -> Bool
		match kmp []     = done kmp 
		match kmp (b':bs') = done kmp || match (next' "isSubstringOf2" kmp b' 1) bs'


makeKmp :: (Show a, Eq a) => [a] -> KMP a
makeKmp xs = trace ("makeKmp " ++ show xs) kmp
	where kmp = makeKmp' xs (\_ _ -> kmp )

makeKmp' :: (Show a, Eq a) => [a] -> (a -> Int -> KMP a) -> KMP a
makeKmp' []     failure = trace "makeKmp' []" KMP {done = True, next = failure, index = 0} -- failure never happens
makeKmp' (x:xs) failure = trace ("makeKmp' " ++ show x ++ ":" ++ show xs) KMP {done = False, next = test, index = 0 }
	where  
		test c i = 
			--trace (show c ++ (if c == x then " == " else " <> ") ++ show x ++ " \t " ++ show xs ++ " -- " ++ show (if c == x then xs else [c])) $
			if c == x then success i else failure c i
		success i = makeKmp' xs (next' "success" (failure x i))

main :: IO ()
main = 
	print $ "homam" `isSubstringOf2` "AhoBhomaChomamI"
	--print $ [5 .. 8::Int] `isSubstringOf2` [1 .. 10]
	--print $ [5 .. 8::Int] `isSubstringOf2` [9 .. 20]

