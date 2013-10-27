-- example:
-- .\tot1 -o homam.svg --width=800 -a="(0.027, 3000)" -b="(0.031, 3000)"

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



-- diagram utility functions
frame :: QDiagram SVG R2 Any
frame = square 1

c1 :: QDiagram SVG R2 Any
c1 = circle 0.5 -- # translate (r2 (0, 0)) # showOrigin

fc1 :: QDiagram SVG R2 Any
fc1 = (frame # lcA (black `withOpacity` 0.0) <> c1 # lcA (black `withOpacity` 0.0)) # showOrigin 



drawRange :: String -> Range Double -> QDiagram SVG R2 Any
drawRange label r@(Range x1 x2) =
	position [(p2 ((x1+x2)/2, 0), ((rect # fcA (red `withOpacity` 0.5)) (x2-x1) 0.2) <> (text label # scale 0.1))]
	 -- <> position [(p2 ((x1+x2)/2, 0), (text label # scale 0.1))]
	 <> position [(p2 (x, 0), fc1 # scale 0.2) | x <- [x1, x2]]


drawInfoText :: Range Double -> Range Double -> QDiagram SVG R2 Any
drawInfoText rx ry = 
	((text $ txt) # scale 0.1)
	where 
		txt = (format2R rx) ++ " ∩ " ++ (format2R ry) ++ " = " ++ (formatPerc2 $ intersectionFraction rx ry)

renderTheDiagram :: Range Double -> Range Double -> QDiagram SVG R2 Any
renderTheDiagram rx@(Range x1 x2) ry@(Range y1 y2) = 
	--(rect 2 1 # lcA (red `withOpacity` 0.1))  <>
	 (
	((position [(p2 (x, 0.0), text (formatPerc2 $ ((x1+y2)/2)+(x*(y2-x1)/2)) # scale 0.05) | x <- [(-1),(-0.8).. 1]])
	<> (position [(p2 (x, 0.1), (rect 0.2 1) # fc silver # scale 0.05) | x <- [(-1),(-0.8).. 1]])
	<> (rect 2 0.2 # lcA (black `withOpacity` 0.0)))
	===
	(drawRange "A" $ fst normalizedXY)
	===
	(drawRange "B" $ snd normalizedXY)
	===
	((rect 2 0.2 # lcA (black `withOpacity` 0.0)) <> (drawInfoText rx ry))
	)
	-- <>
	---- axis
	--(position [(p2 (x, 0.2), (rect 0.2 1) # fc silver # scale 0.05) | x <- [(-1),(-0.8).. 1]])
	-- <>
	--(position [(p2 (x, 0.27), text (formatPerc2 $ ((x1+y2)/2)+(x*(y2-x1)/2)) # scale 0.05) | x <- [(-1),(-0.8).. 1]])
	where
		normalizedXY = normalizeRs (rx, ry)


format2 :: Double -> String
format2 n = show ((fromIntegral $ round $ (n * 10000))/100)

formatPerc2 :: Double -> String
formatPerc2 = (++"%") . format2 --(++"%") $ show ((fromIntegral $ round $ (n * 10000))/100)

format2R :: Range Double -> String
format2R (Range rx ry) = "(" ++ format2 rx ++ " .. " ++ format2 ry ++ ")"


--- input
data DiagramOpts = DiagramOpts
	{ width     :: Maybe Int
	, height    :: Maybe Int
	, output    :: FilePath
	, selection :: Maybe String
	, a		:: String
	, b 		:: String
	}
	deriving (Show, Data, Typeable)

diagramOpts :: String -> Bool -> DiagramOpts
diagramOpts prog sel = DiagramOpts
  { width =  def
             &= typ "INT"
             &= help "Desired width of the output image"

  , height = def
             &= typ "INT"
             &= help "Desired height of the output image"

  , output = def
           &= typFile
           &= help "Output file"

  , selection = def
              &= help "Name of the diagram to render"
              &= (if sel then typ "NAME" else ignore)
  , a = def
  , b = def
  }
  &= summary "Command-line diagram generation."
  &= program prog


main ::  IO ()
main = do
	prog <- getProgName
	args <- getArgs
	opts <- cmdArgs (diagramOpts prog False)
	--putStrLn $ show opts
	let (probA, sizeA) = (read (a opts)) :: (Double, Double)
	let (probB, sizeB) = (read (b opts)) :: (Double, Double)
	putStrLn (show (probA, sizeA))
	putStrLn (show (probB, sizeB))

	putStrLn $ show $ width opts

	let d = renderTheDiagram (prob probA sizeA) (prob probB sizeB)
	let
		build = renderDia SVG (SVGOptions (Width 800) Nothing) d
    	BS.writeFile (output opts) (renderSvg build)
	putStrLn "Done!"






-- datatype Range 

