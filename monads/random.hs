import System.Random
import Control.Monad.State

-- random :: (RandomGen g, Random a) => g -> (a, g)  

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
	a <- randomSt
	b <- randomSt
	c <- randomSt
	return (a,b,c)

main :: IO ()
main = do
	print " --- "
	print $ runState threeCoins (mkStdGen 10)
	print $ runState liftM randomSt $ mkStdGen 10