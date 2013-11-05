
{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude hiding (width, height, interval)
import Diagrams.Backend.SVG.CmdLine
import System.Random
import Normal



normalizeCRatesTuples :: [(Double, Int)] -> [(Double, Double)]
normalizeCRatesTuples tups = [(label, norm x) | (label, x) <- tups]
	where 
		norm x = fromIntegral x / mx
		mx = fromIntegral $ maximum $ map snd tups


bar :: (Double,Double) -> QDiagram SVG R2 Any 
bar (label,h) = rect 0.1 h # lc black === (text (formatPerc2 label) # scale 0.02 <> rect 0.1 0.1) -- rect 1 1 # lcA (green `withOpacity` 0.2) <> 
	--where h' = fromIntegral h
--d = bar 1.0

main :: IO ()
main = do
	
	gen <- newStdGen
	--let (coins, gen') = randomValues 10000 gen :: ([Bool], StdGen)
	---- 							bins trial size ds gen
	--let cRatesTuples = normalTest 25 10000 50 coins gen'
	--print $ conversionRate coins

	putStrLn "--"

	contents <- readFile "iraq.csv"
	let ds = maybeTupleToConversionList $ parse contents	
	let cRatesTuples = normalTest 10 1000 500 ds gen
	print $ conversionRate ds

	putStrLn "--"

	defaultMain $ hcat . map (alignB . bar) $ normalizeCRatesTuples cRatesTuples --[bar h | h <- normalizeSizes sizes]






format2 :: Double -> String
format2 n = show $ fromIntegral(round (n * 10000))/100

formatPerc2 :: Double -> String
formatPerc2 = (++"%") . format2 