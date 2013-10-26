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


-- | 95 % confidence interval
prob :: (Floating a, Ord a) => a -> a -> Range a
prob p n = range (p - c) (p + c)
	where
		c = 1.96 * sqrt(p * (1-p)/n) / 2

-- diagram utility functions
frame = square 1
c1 = circle 0.5 -- # translate (r2 (0, 0)) # showOrigin

fc1 = (frame <> c1) # showOrigin 

drawRange r@(Range x1 x2) =
	position [(p2 ((x1+x2)/2, 0), (rect # fcA (blue `withOpacity` 0.5)) (x2-x1) 0.2)] <>
	position [(p2 (x, 0), fc1 # scale 0.2) | x <- [x1, x2]]





tupleToProbRanges :: (Floating a, Ord a) => (a, a) -> (a, a) -> ((Range a), (Range a))
tupleToProbRanges (probA, sizeA) (probB, sizeB) = ((prob probA sizeA), (prob probB sizeB))


rangeA :: Range Double
rangeA = prob 0.039 3000

rangeB :: Range Double
rangeB = prob 0.042 3000

normalizedAB :: (Range Double, Range Double)
normalizedAB = normalizeRs (rangeA, rangeB)
--example = rect 2 1 # lc purple



---- 
example = (rect 2 1 # lc purple) <> (
	(drawRange $ fst normalizedAB)
	===
	(drawRange $ snd normalizedAB)
	)
	<>
	(position [(p2 (x, 0), (rect 0.2 1) # fc silver # scale 0.05) | x <- [(-1),(-0.8).. 1]])


--renderTheDiagram rx@(Range x1 x2) ry@(Range y1 y2) = example

renderTheDiagram :: Range Double -> Range Double -> QDiagram SVG R2 Any
renderTheDiagram rx@(Range x1 x2) ry@(Range y1 y2) = 
	(rect 2 1 # lc purple)  <> (
	(drawRange $ fst normalizedXY)
	===
	(drawRange $ snd normalizedXY)
	)
	<>
	-- axis
	(position [(p2 (x, 0), (rect 0.2 1) # fc silver # scale 0.05) | x <- [(-1),(-0.8).. 1]])
	where
		normalizedXY = normalizeRs (rx, ry)


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
	putStrLn $ show opts
	let (probA, sizeA) = (read (a opts)) :: (Double, Double)
	let (probB, sizeB) = (read (b opts)) :: (Double, Double)
	putStrLn (show (probA, sizeA))
	putStrLn (show (probB, sizeB))

	--putStrLn (show example)
	
	--let d = position [(p2 (-0.5,-0.45), (alignedText 0 0 (show normalizedAB) # scale 0.05)), (p2 (0,0), example)]
	--let d = renderTheDiagram 2

	--let go = renderSvg $ renderDia SVG (SVGOptions (Width 800) Nothing) d

	let d = renderTheDiagram (prob probA sizeA) (prob probB sizeB)
	let
		sizeSpec :: SizeSpec2D
		sizeSpec = Width 800
		--options :: Options SVG R2 
		--options = SVGOptions sizeSpec
		build = renderDia SVG (SVGOptions (Width 800) Nothing) d
    	BS.writeFile "c.svg" (renderSvg build)
	--defaultMain d
	putStrLn "a"






data Range a = Range a a deriving (Read)

range :: Ord a => a -> a -> Range a
range x y = Range (min x y) (max x y)

instance Show a => Show (Range a) where
	show (Range x1 x2) = "(" ++ (show x1) ++ ", " ++ (show x2) ++ ")"

instance Eq a => Eq (Range a) where
	(Range x1 x2) == (Range y1 y2) = x1 == y1 && x2 == y2

instance Ord a => Ord (Range a) where
	(Range x1 x2) `compare` (Range y1 y2) = x1 `compare` y1


lenR :: Num a => Range a -> a
lenR (Range x1 x2) = x2 - x1

transformR :: Num a => a -> Range a -> Range a
transformR n (Range x1 x2) = Range (x1 + n) (x2 + n)

normalizeRs :: (Fractional a, Ord a) => (Range a, Range a) -> (Range a, Range a)
normalizeRs (x@(Range x1 x2), y@(Range y1 y2)) = (rnorm x, rnorm y)
	where 
		rnorm (Range w z) = Range (norm w) (norm z)
		norm v = (v - m) / l
		m = ((maximumR [x, y]) + (minimumR [x, y])) / 2
		l = ((maximumR [x, y]) - (minimumR [x, y])) / 2


intersectionR :: (Num a, Ord a) => Range a -> Range a -> Range a
intersectionR r1 r2 = _intersectionR (min r1 r2) (max r1 r2)
	where _intersectionR (Range x1 x2) (Range y1 y2)
		| x2 < y1 = Range 0 0
		| otherwise = Range (min y1 x2) (max y1 x2)

unionR :: (Ord a, Num a) => Range a -> Range a -> [Range a]
unionR rx ry
	| intersectionR rx ry == Range 0 0 =  [rx, ry]
	| otherwise = [Range (minimumR [rx, ry]) (maximumR [rx, ry])]



minimumR :: (Ord a) => [Range a] -> a
minimumR rs = foldl1 (min) $ [min x1 x2 | (Range x1 x2) <- rs]

maximumR :: (Ord a) => [Range a] -> a
maximumR rs = foldl1 (max) $ [max x1 x2 | (Range x1 x2) <- rs]


rangeToList :: Range a -> [a]
rangeToList (Range x1 x2) = [x1, x2]
--minimumRs :: (Ord a) => a -> [Range a] -> a
--minimumRs v [] = v
--minimumRs v (r:rs) = minimumRs (minR r v) rs 
--	where minR (Range x1 x2) v = minimum [x1, x2, v]

