import Control.Applicative


-- | Factorial function

fact :: (Eq a, Num a) => a -> a
fact 0 = 1
fact n = n * fact (n-1) 


-- | 'normal' returns the value of Normal distribution function
-- for @sigma@ = standard deviation and @mu@ = mean for any @x@

normal :: Floating a => a -> a -> a -> a
normal sigma mu x = c * exp(eup / edown) where
	c = 1/(sigma*sqrt(2 * pi))
	eup = (x - mu)**2
	edown = -2 * (sigma**2)



-- | 'binomial' returns the value of Binomal distribution function 
-- for @s@ successes out of @n@ total trials at @x < n@

binomial :: (Fractional b, Integral a) => a -> a -> a -> b
binomial s n x = 1 / fromIntegral (down `div` up)  where -- `div` down where
	up = fact n * (s^x) * ((n-s)^(n-x))
	down = fact x * fact (n -x ) * (n^x) * (n^(n-x))


-- | 'binomialNormal' returns the value of Normal approximation for a
-- Binomial distribution with @s@ successes out of @n@ total trials at @x < n@
binomialNormal :: (Floating b, Integral a) => a -> a -> b -> b
binomialNormal s n = normal sigma mu where
	[n', s'] = fromIntegral <$> [n, s]
	p = s'/n'
	mu = n' * p
	sigma = sqrt $ n' * p * (1 - p)


-- | 'binomialConf' returns the sum of absolute difference between two Binomial distributions
-- both with @n@ trials, but one with @a@ succcesses and the other one with @b@ successes.
-- 
-- Use 'binomialNormalConf' for an approximation but faster result.

binomialConf :: (Fractional b, Integral a) => a -> a -> a -> b
binomialConf n a b = sum [ abs (binomial a n i - binomial b n i) | i <- [0..n]] / 2


-- | 'binomialNormalConf' returns the sum of absolute difference between 
-- Normal approximations of two Binomial distributions
-- both with @n@ trials, but one with @a@ succcesses and the other one with @b@ successes.

binomialNormalConf :: (Floating b, Integral a) => a -> a -> a -> b
binomialNormalConf n a b =  sum [ abs (binomialNormal a' n' (fromIntegral i) - binomialNormal b' n' (fromIntegral i)) | i <- [0..n]] / 2
	where
		[n', a', b'] = fromIntegral <$> [n, a, b]


binomialResult, binomialNormalResult :: (Integer, Integer) -> (Integer, Integer) -> (Double, Double, Double, Double)


binomialResult = result binomialConf
	
binomialNormalResult = result binomialNormalConf




result :: (Floating b, Integral a) => 
	(a -> a -> a -> b) -> (a, a) -> (a, a) -> (Double, Double, Double, b)
result confFunc (na, a) (nb, b) = (aConv, bConv, diff, confFunc (floor n) (floor a'') (floor b'')) where
	[a', b', na', nb'] = fromIntegral <$> [a,b,na,nb]
	n = max na' nb'
	ra = n / na'
	rb = n / nb'
	a'' = a' * ra
	b'' = b' * rb
	aConv = a' / na'
	bConv = b' / nb'
	diff = abs (aConv - bConv) / min aConv bConv



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
-- ( sum [ abs ((y 85 155 i) - (y 80 155 i)) | i <- [0..155]] ) / 2
