
{-# XNoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable, CPP #-} -- DeriveDataTypeable,

import System.Environment  (getArgs, getProgName)
import Diagrams.Prelude hiding (width, height, interval)
import Data.Colour (withOpacity)

import qualified Data.ByteString.Lazy as BS
import Diagrams.Backend.SVG
import System.Console.CmdArgs.Implicit hiding (args)
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)

import Diagrams.Backend.SVG.CmdLine

import Data.List (sort)
import Range

type DRange = Range Double


format2 :: Double -> String
format2 n = show $ fromIntegral(round (n * 10000))/100

formatPerc2 :: Double -> String
formatPerc2 = (++"%") . format2 

format2R :: DRange -> String
format2R (Range rx ry) = "(" ++ format2 rx ++ " .. " ++ format2 ry ++ ")"

-- makes the border transparent
lcTransparent :: QDiagram SVG R2 Any -> QDiagram SVG R2 Any
lcTransparent = lcA (black `withOpacity` 0.0)

tick :: Double -> QDiagram SVG R2 Any
tick v = 
	((rect 0.2 0.6 # lcTransparent) <> (text (format2 v) # scale 0.4))
	===
	(rect 0.1 0.5 # fc black)


-- drawRangeRect is indipendent of position
drawRangeRect :: Double -> String -> Double -> QDiagram SVG R2 Any 
drawRangeRect height label width =
	rect width height # fcA (purple `withOpacity` 0.5)
	<>
	(text label # scale 0.1)

drawRange :: Double -> String -> Range Double -> QDiagram SVG R2 Any
drawRange height label (Range x1 x2) =
	position [(p2 ((x1+x2)/2, 0), drawRangeRect height label (x2-x1))]


-- layerHeight = 1 / numebr of vertically stacked diagrams 

stackR layerHeight g = rect 2.1 layerHeight # lcA (black `withOpacity` 0.1) <> g
	
axisTickLabel :: Double -> Double -> Double -> Double
axisTickLabel minx maxx x = ((x1+y2)/2)+(x*(y2-x1)/2)
	where
		x1 = minx
		y2 = maxx

axis :: Double -> Double -> [(P2, QDiagram SVG R2 Any)]
axis minx maxx = [(p2 (x, 0), tick (axisTickLabel minx maxx x) # scale 0.1) | x <- [(-1),(-0.8).. 1]]


barsDiagram :: [Range Double] -> QDiagram SVG R2 Any
barsDiagram ranges = 
	stackRHeight (position $ axis (min minx1 minx2) (max maxx1 maxx2))
	=== 
	foldedRanges
	=== 
	foldedRows
	--stackRHeight (rect 2.1 layerHeight)
	where
		layerHeight = 1.0/fromIntegral(length ranges + 1)
		stackRHeight = stackR layerHeight

		-- ranges graphs
		normalizedRanges = normalizeRList ranges
		stackRHeightRange label = stackRHeight . drawRange layerHeight label
		foldedRanges = foldl (===) mempty [stackRHeightRange [label] r | (r, label) <- zip normalizedRanges ['A'..'Z']]

		-- extends of the graph
		sranges = sort ranges
		(Range minx1 minx2) = head sranges
		(Range maxx1 maxx2) = last sranges

		-- table
		intersfractions = [[intersectionFraction x y | x <- ranges] | y <- ranges]
		stackHeightRow cols = stackRHeight (foldedCols cols)
		foldedCols cols = foldl (|||) mempty [(rect 0.4 0.2) <> text (formatPerc2 c) # scale 0.05 | c <- cols]
		foldedRows = foldl (===) mempty [stackHeightRow r | r <- intersfractions]
		-- foldedRows = (rect 2.1 0.2 # lc red ) <> ((rect 0.4 0.3) ||| (rect 0.4 0.2))


main :: IO ()
main = do
	let probs = [prob 0.035 2587, prob 0.034 2787, prob 0.042 2882, prob 0.031 2301]
	putStrLn $ show [[intersectionFraction x y | x <- probs] | y <- probs]
	defaultMain $ barsDiagram  probs -- drawRangeRect "A" 2 --((rect 2 1) <> (text "A" # scale 1 ))
