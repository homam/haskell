
{-# XNoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable, CPP #-}

import System.Environment  (getArgs, getProgName)
import Diagrams.Prelude hiding (width, height, interval)
import Data.Colour (withOpacity)

import qualified Data.ByteString.Lazy as BS
import Diagrams.Backend.SVG
import System.Console.CmdArgs.Implicit hiding (args)
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)

import Diagrams.Backend.SVG.CmdLine

import Range

type DRange = Range Double


format2 :: Double -> String
format2 n = show ((fromIntegral $ round $ (n * 10000))/100)

formatPerc2 :: Double -> String
formatPerc2 = (++"%") . format2 --(++"%") $ show ((fromIntegral $ round $ (n * 10000))/100)

format2R :: Range Double -> String
format2R (Range rx ry) = "(" ++ format2 rx ++ " .. " ++ format2 ry ++ ")"



lcTransparent = lcA (black `withOpacity` 0.0)

tick :: Double -> QDiagram SVG R2 Any
tick v = 
	((rect 0.2 0.6 # lcTransparent) <> (text (format2 v) # scale 0.4))
	===
	(rect 0.1 0.5 # fc black)


-- drawRangeRect is indipendent of position
drawRangeRect :: Double -> String -> Double -> QDiagram SVG R2 Any 
drawRangeRect height label width =
	(rect width height) # fcA (red `withOpacity` 0.5)
	<>
	(text label # scale 0.1)

drawRange :: Double -> String -> Range Double -> QDiagram SVG R2 Any
drawRange height label r@(Range x1 x2) =
	position [(p2 ((x1+x2)/2, 0), drawRangeRect height label (x2-x1))]


-- layerHeight = 1 / numebr of vertically stacked diagrams 

layerHeight = 0.25

stackR g = (rect 2.1 layerHeight # lcA (black `withOpacity` 0.1)) <> g

rangeA :: DRange
rangeA = prob 0.035 2587

rangeB :: DRange
rangeB = prob 0.029 2687

normalizedXY :: (DRange, DRange)
normalizedXY = normalizeRs (rangeA, rangeB)

	
axisTickLabel :: Double -> Double
axisTickLabel x = ((x1+y2)/2)+(x*(y2-x1)/2)
	where
		(Range x1 x2) = min rangeA rangeB
		(Range y1 y2) = max rangeA rangeB

axis = [(p2 (x, 0), tick (axisTickLabel x) # scale 0.1) | x <- [(-1),(-0.8).. 1]]

d = 
	(
		(stackR $ position axis)
		===
		(stackR $ drawRange layerHeight "A" (fst normalizedXY))
		===
		(stackR $ drawRange layerHeight "B" (snd normalizedXY))
		===
		(stackR $ drawRange layerHeight "C" (snd normalizedXY))
		===
		(stackR $ drawRange layerHeight "D" (snd normalizedXY))
	)

main = defaultMain $ d -- drawRangeRect "A" 2 --((rect 2 1) <> (text "A" # scale 1 ))