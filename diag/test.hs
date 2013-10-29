
{-# XNoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable, CPP #-} -- DeriveDataTypeable,

import System.Environment  (getArgs, getProgName)
import Diagrams.Prelude hiding (width, height, interval)


import qualified Data.ByteString.Lazy as BS
import Diagrams.Backend.SVG
import System.Console.CmdArgs.Implicit hiding (args)
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)

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

stackLayerWithHeight layerHeight g = rect 2.1 layerHeight # lcA (black `withOpacity` 0.1) <> g
	
axisTickLabel :: Double -> Double -> Double -> Double
axisTickLabel minx maxx x = ((x1+y2)/2)+(x*(y2-x1)/2)
	where
		x1 = minx
		y2 = maxx

axis :: Double -> Double -> [(P2, QDiagram SVG R2 Any)]
axis minx maxx = [(p2 (x, 0), tick (axisTickLabel minx maxx x) # scale 0.1) | x <- [(-1),(-0.8).. 1]]


barsDiagram :: [Range Double] -> QDiagram SVG R2 Any
barsDiagram ranges = 
	stackLayer (position $ axis (min minx1 minx2) (max maxx1 maxx2))
	=== 
	foldedRanges
	|||
	foldedRows
	--stackLayer (rect 2.1 layerHeight)
	where
		layerHeight = 1.0/fromIntegral(length ranges + 1)
		stackLayer = stackLayerWithHeight layerHeight

		-- ranges graphs
		normalizedRanges = normalizeRList ranges
		stackLayerRange label = stackLayer . drawRange layerHeight label -- String -> Range Double -> Diagram
		foldedRanges = foldl (===) mempty [stackLayerRange [label] r | (r, label) <- normalizedRanges `zip` ['A'..'Z']]

		-- extends of the graph
		sranges = sort ranges
		(Range minx1 minx2) = head sranges
		(Range maxx1 maxx2) = last sranges

		-- table
		cellWidth = 2.1/fromIntegral(length ranges + 1)
		cell label = rect cellWidth layerHeight <> text label # scale 0.05 -- cell diagram with the text label at its center
		table = [[intersectionFraction x y | x <- ranges] | y <- ranges]
		-- data row columns
		foldedCols label cols = centerX $ foldl (|||) mempty [cell val | val <- label : map formatPerc2 cols]
		stackLayerRow label cols = stackLayer (foldedCols label cols)
		foldedRows = 
			-- header
			centerX (foldl (|||) mempty [cell [label] | label <- (" "++) $ map snd $ table `zip` ['A'..]])
			===	
			-- rows
			foldl (===) mempty [stackLayerRow [label] r | (r,label) <- table `zip` ['A'..]]


-- usage:
-- ./test -o "t.svg" --width=1100 --height=500  --list="[(0.035, 2587), (0.034, 2787), (0.042, 2882), (0.031, 2301), (0.029, 2431)]"

main :: IO ()
main = do 
	prog <- getProgName
	args <- getArgs
	opts <- cmdArgs (diagramOpts prog)
	let rangeProbs = read(list opts)::[(Double, Double)]
	let probs = [prob p n | (p, n) <- rangeProbs] 
	let d = barsDiagram probs

	let sizeSpec = case (width opts, height opts) of
                            (Nothing, Nothing) -> Absolute
                            (Just w, Nothing)  -> Width (fromIntegral w)
                            (Nothing, Just h)  -> Height (fromIntegral h)
                            (Just w, Just h)   -> Dims (fromIntegral w)
                                                       (fromIntegral h)

	let
		build = renderDia SVG (SVGOptions sizeSpec Nothing) d
    	BS.writeFile (output opts) (renderSvg build)
	putStrLn "Done!"





--- input
data DiagramOpts = DiagramOpts
	{ width     :: Maybe Int
	, height    :: Maybe Int
	, output    :: FilePath
	, list		:: String
	}
	deriving (Show, Data, Typeable)

diagramOpts :: String -> DiagramOpts
diagramOpts prog = DiagramOpts
  { width =  def
             &= typ "INT"
  , height = def
             &= typ "INT"
  , output = def
           &= typFile
  , list = def
  }