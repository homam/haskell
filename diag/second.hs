{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
--import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.Cairo.CmdLine

no :: (Semigroup b, TrailLike b, Transformable b, HasStyle b, V b ~ R2) => b
no = (circle 1 <> hrule 2 # rotateBy (1/8)) # lw 0.2 # lc red

example :: (Renderable (Path R2) b, Renderable Image b) => Diagram b R2
example = no <> image "./hello.png" 2 2

main :: IO ()
main = mainWith (ex :: Diagram B R2)
-- main = mainWith (rect 2.5 2.5 <> example :: Diagram B R2) -- (example :: Diagram B R2) -- $ no <> image "" 1.5 1.5


ex = (square 1
	||| square 1 # scale 2
	||| circle 1 # scaleX 3) # lw 0.2 # dashing [0.1,0.1] 0