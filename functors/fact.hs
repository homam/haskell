{-# LANGUAGE BangPatterns #-}

import Control.Applicative

fact 0 = 1
fact n = n * fact (n-1) 


inv n = 1 / n



y p n x = (fromIntegral y1) where  -- 1 / (fromIntegral y1)
		y1 = ((fact . truncate $ x)*(fact . truncate $ n-x)) `div` ((fact . truncate $ n) `div` (truncate . inv $ ( p**x * (1-p)**(n-x) ))) 



choose n k = (fact n) `div` ( (fact k) * (fact (n-k)) )

cross a b = 1 / fromIntegral (b `div` a)

times a 1 = a
times a !n = (a +) $! (times a $! (n-1))


-- y' p n x = (choose (truncate n) (truncate x)) * (p**x) * (1-p)**(n-x)

y' p n x = 1 / (fromIntegral up) where
	up = (truncate ( 1 / ( (p**x) * (1-p)**(n-x) ) ) * (fact . truncate $ x) * (fact . truncate $ n-x) )  `div` (fact . truncate $ n)


y'' p n x =  1 / (fromIntegral up) where
	up = (truncate ( 1 / ( (p**(fromIntegral x)) * (1-p)**((fromIntegral n)-(fromIntegral x)) ) ) * (fact x) * (fact $ n-x) )  `div` (fact n)




-- this one is the done to modify
y''' s n x = 1 / fromIntegral (down `div` up)  where -- `div` down where
	up = (fact n) * (s^x) * ((n-s)^(n-x))
	down = (fact x) * (fact $ n-x) * (n^x) * (n^(n-x))



-- plot (( abs (100! * 25^x * (75)^(100-x) -  (100! * 22^x * (78)^(100-x))) / (x! * (100-x)! * 100^100 )) for x = 0 to 100
--	plot (( abs (155! * 85^x * (15)^(155-x) -  (155! * 80^x * (20)^(155-x))) / (x! * (155-x)! * 155^155 )) for x = 0 to 155

-- wolfram: 
-- sub = (n, a, b) -> "sum ( abs ((#{n}! * #{a}^x * (#{n - a})^(#{n}-x)) - (#{n}! * #{b}^x * (#{n - b})^(#{n}-x))) / (x! * (#{n}-x)! * #{n}^#{n} ) ) from x = 0 to #{n}"



-- how much we're confident that the test results are goin to hold:
-- for example: we're 75% confident that there will be a 15% difference between cases A and B.

-- wolfram alpha versions (use sub 100 20 80)
--formula = (n, a) -> "sum ( (#{n}! * #{a}^x * (#{n - a})^(#{n}-x)) / (x! * (#{n}-x)! * #{n}^#{n} ) from x = 0 to #{n}"
--y = (n, a) -> "( (#{n}! * #{a}^x * (#{n - a})^(#{n}-x)) / (x! * (#{n}-x)! * #{n}^#{n} )"
--formula = (n, a) -> "sum (#{y n, a}) from x = 0 to #{n}"
--sub = (n, a, b) -> "sum (abs ( #{y n, a} - #{y n, a})) from x = 0 to #{n}"


-- in haskell:
-- ( sum [ abs ((y''' 85 155 i) - (y''' 80 155 i)) | i <- [0..155]] ) / 2

conf n a b = ( sum [ abs ((y''' a n i) - (y''' b n i)) | i <- [0..n]] ) / 2

result (na, a) (nb, b) = (aConv, bConv, diff, conf (floor n) (floor a'') (floor b'')) where
	[a', b', na', nb'] = fromIntegral <$> [a,b,na,nb]
	n = max na' nb'
	ra = n / na'
	rb = n / nb'
	a'' = a' * ra
	b'' = b' * rb
	aConv = a' / na'
	bConv = b' / nb'
	diff = (abs $ aConv - bConv) / (min aConv bConv)
	





binomialNormalConf n a b =  ( sum [ abs ((binomialNormal a' n' (fromIntegral i)) - (binomialNormal b' n' (fromIntegral i))) | i <- [0..n]] ) / 2
	where
		[n', a', b'] = fromIntegral <$> [n, a, b]

binomialResult (na, a) (nb, b) = (aConv, bConv, diff, binomialNormalConf (floor n) (floor a'') (floor b'')) where
	[a', b', na', nb'] = fromIntegral <$> [a,b,na,nb]
	n = max na' nb'
	ra = n / na'
	rb = n / nb'
	a'' = a' * ra
	b'' = b' * rb
	aConv = a' / na'
	bConv = b' / nb'
	diff = (abs $ aConv - bConv) / (min aConv bConv)



normal sigma mu x = c * exp(eup / edown) where
	c = 1/(sigma*sqrt(2 * pi))
	eup = (x - mu)**2
	edown = -2 * (sigma**2)

binomialNormal s n = normal sigma mu where
	[n', s'] = fromIntegral <$> [n, s]
	p = s'/n'
	mu = n' * p
	sigma = sqrt $ n' * p * (1 - p)







y'''' s n x =  (fromIntegral up) / (fromIntegral down) where -- (down `div` up)  where -- 1 / fromIntegral (down `div` up)  where -- `div` down where
	up = (fact n) * (s^x) * ((n-s)^(n-x))
	down = (fact x) * (fact $ n-x) * (n^x) * (n^(n-x))


