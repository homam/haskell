{-# LANGUAGE FlexibleContexts #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Graphics.SVGFonts.ReadFont
import Diagrams.Core.Points -- needed to work around bug in GHC 7.4
import Diagrams.Backend.SVG.CmdLine

type Points = [(Double, Double)]

xseries = [(-3), (-2.9) .. 3]

dataSeries :: [(String,Points)]
dataSeries =
  [ 
  --("upward",   zip [0.0, 1.0 .. 10.0] [0.0, 1.0 .. 10.0])
  --, ("downward", zip [0.0, 1.0 .. 10.0] [10.0, 9.0 .. 0.0])
  --, ("cycle",    zip [0.0, 1.0 .. 10.0] (cycle [3,4,5]))
  --, ("arbitrary", [(2,4), (4,2), (5,4), (10,5)])
  --  ("sin",      map (\x -> (x, sin x)) xseries)

    ("1-1", [((-0.5), 0.5), (0.5, 0.5)])
  , ("cdf .32", map (cdf' 0.0 0.2) xseries)
  , ("cdf' .1", map (cdf' 0.2 1.0) xseries)
  ]


--cdf :: Floating a => a -> a
cdf mu sigma x = (x, y)
  where
    dxmu = (x-mu)
    sigma2 = sigma*sigma
    sqrt2pi = sqrt (2*pi)
    y = exp( dxmu*dxmu / ((-2)*sigma2) ) / (sigma * sqrt2pi)

cdf' mu sigma2 x = (x, y)
  where
    dxmu = (x-mu)
    y = exp( dxmu*dxmu / ((-2)*sigma2) ) / (sqrt (2*pi*sigma2))

type Dia = QDiagram SVG R2 Any -- Diagram B R2

example :: Dia
example = centerXY $
    (
      centerY (chart (map snd dataSeries) plotStyles [(-3),(-2) .. 3] [(-1),(0) .. 2])
      ||| strutX 1
    -- ||| centerY (legend plotStyles (map fst dataSeries))
    )
     `atop` square 12 # translateX (w/2) -- # scaleY 0.85 -- border


-- The size of the chart, in logical units.

h,w :: Double
h = 7
w = 7


trials1 = 1000
success1 = 0.04

trials2 = 500
success2 = 0.08

variance p n = p * (1 - p) * n
mean p n = p * n


chart :: [Points] -> [(Dia, Dia -> Dia)] -> [Double] -> [Double] -> Dia
chart series styles xs ys = mconcat
	[ 
    plotMany styles series dataToFrac -- data series
  , square 1 # moveTo (w^&h)
  -- , histogram (map (cdf' 0.0 0.2) xseries) (fcA (blue `withOpacity` 0.5)) dataToFrac
  -- , histogram (map (cdf' 0.4 0.3) xseries) (fcA (yellow `withOpacity` 0.5)) dataToFrac
  , histogram (map (cdf' 0.5 0.1) xseries) (fcA (yellow `withOpacity` 0.5)) dataToFrac
	, horizticks (map (\x -> ((x-minx)/xrange, showFloor x)) xs) -- h axis
	, vertticks  (map (\y -> ((y-miny)/yrange, showFloor y)) ys) -- v axis
	, box -- outer box
	]
	where 
		maxx = last xs
		minx = head xs
		maxy = last ys
		miny = head ys
		xrange = maxx-minx
		yrange = maxy-miny
		dataToFrac (x,y) = ((x-minx)/xrange, (y-miny)/yrange) -- converts points from the “data” space [-3..3] into the [0..1] range.
		showFloor = show . (floor :: Double -> Integer)



-- histogram :: HasStyle a => [(Double,Double)] -> (a -> a)  -> ((Double,Double) -> (Double,Double)) -> Dia
histogram :: (Renderable (Path R2) b, Monoid a) =>
  [(Double,Double)] -> (Diagram b R2 -> a) -> ((Double,Double) -> (Double,Double)) -> a
histogram series styles dataToFrac = 
  mconcat (map rectg series)
  where
    scalify (x,y) = (x*w,y*h)
    psf = (p2 . scalify . dataToFrac)
    dx = 0.1
    rect' h = map psf [(0,0),(dx, 0),(dx,h),(0,h)]
    rectg (x,y) = ((strokeLoop . closeLine . fromVertices) (rect' y)) # moveTo (psf ((x-dx/2)^&0)) # styles -- fcA (blue `withOpacity` 0.5)



-- Plot a single data series. A “shape” is drawn at every data point, and straight lines are drawn between points.

plot :: ((Double,Double) -> (Double,Double)) -> Dia -> (Dia -> Dia) -> [(Double,Double)] -> Dia
plot dataToFrac shape lineStyle ps =
    let scalify (x,y) = (x*w,y*h)
        ps' = map (p2 . scalify . dataToFrac) ps
    in (stroke $ fromVertices ps') # lineStyle
         `beneath` mconcat [ shape # moveTo p | p <- ps' ]


--Plot many data series using the given list of styles.

plotMany :: [(Dia, Dia -> Dia)] -> [[(Double, Double)]] -> ((Double, Double) -> (Double, Double)) -> Dia
plotMany styles seriesList dataToFrac =
    mconcat $ zipWith (uncurry (plot dataToFrac)) (styles ++ plotStyles) seriesList


legend :: [(Dia, Dia -> Dia)] -> [String] -> Dia
legend styles labels = centerXY $
	vcat' with {_sep=0.15} $
      map (\(l,s) -> littleLine s ||| strutX 0.4 ||| text l # alignL # fontSize 0.5)
        (zip labels (styles ++ plotStyles))
	where littleLine (d,l) = (stroke $ fromVertices [ 0^&0, 1^&0 ]) # l
                           <> d # moveTo (0.5^&0)


--The outer box is just a rectangle.

box :: Dia
box = strokeLoop . closeLine . fromVertices $ [ 0^&0, 0^&h, w^&h, w^&0 ]


-- Each tick on the vertical or horizontal axis has a text part,
--  a solid line on the left, a solid line on the right, and a long dashed line from left to right.
vertticks :: [(Double,String)] -> Dia
vertticks pairs =
    let textBits = mconcat [ text t # alignR # moveTo ((-0.2)^&(y*h)) # fontSize 0.5  | (y,t) <- pairs ]
        tickBits =    mconcat [ fromVertices [ 0^&(y*h), 0.1    ^&(y*h) ] | (y,_) <- pairs ]
                   <> mconcat [ fromVertices [ w^&(y*h), (w-0.1)^&(y*h) ] | (y,_) <- pairs ]
                   <> mconcat [ fromVertices [ 0^&(y*h), w^&(y*h)       ] # lc gray # dashing [ 0.1, 0.1 ] 0 | (y,_) <- pairs ]
    in textBits <> tickBits


horizticks :: [(Double,String)] -> Dia
horizticks pairs =
    let textBits = mconcat [ text t # moveTo ((x*w)^&(-0.3)) # fontSize 0.5 | (x,t) <- pairs ]
        tickBits =    mconcat [ fromVertices [ (x*w)^&0, (x*w)^&0.1     ] | (x,_) <- pairs ]
                   <> mconcat [ fromVertices [ (x*w)^&h, (x*w)^&(h-0.1) ] | (x,_) <- pairs ]
                   <> mconcat [ fromVertices [ (x*w)^&0, (x*w)^&h       ] # lc gray # dashing [ 0.1, 0.1 ] 0 | (x,_) <- pairs ]
    in textBits <> tickBits


--A dot style is a shape (any diagram) and a boolean indicating whether the shape should be filled, a line style is a dashing pattern, and a colour style is just a colour. These three combined give a “style”.

newtype Fill = Fill Bool
type Shape = Dia
type DotStyle = (Shape, Fill)
type LineStyle = Dia -> Dia

plotStyles :: [ (Shape, LineStyle) ]
plotStyles = zipWith3 combineStyles dotStyles colourStyles lineStyles

combineStyles :: DotStyle -> Colour Double -> LineStyle -> (Shape, LineStyle)
combineStyles (d,Fill f) c l =
  ( d # (if f then fcA (c `withOpacity` 0.5) else id) # lc c, lc c . l )


--The dot styles.

dotStyles :: [DotStyle]
dotStyles = cycle $
    let shapes = map (stroke)
           [ circle 0.07
           , square 0.1
           , eqTriangle 0.1
           , pentagon 0.1
           , cross 0.07
           , plus 0.07
           , star (StarSkip 2) (pentagon 0.1)
           ]
    in [ (s, Fill b) | b <- [True,False], s <- shapes ]


--Some custom shapes.

cross :: Double -> Path R2
cross x = fromVertices [ x^&(-x) , ((-x)^&x) ]
          <> fromVertices [ x^&x , ((-x)^&(-x)) ]

plus :: Double -> Path R2
plus x = cross x # rotate (45::Deg)


--The colour styles.

colourStyles :: [Colour Double]
colourStyles = cycle $ [ red, green, blue, brown ]


--The line styles.

lineStyles :: [Dia -> Dia]
lineStyles = cycle . map (. lw 0.03) $
               [ id, dashing [0.1,0.1] 0, dashing [0.02,0.02] 0
               , dashing [0.1,0.1,0.03,0.1] 0, dashing [0.1,0.1,0.02,0.02,0.02,0.1] 0 ]



main :: IO ()
main = mainWith (example  :: Diagram B R2)