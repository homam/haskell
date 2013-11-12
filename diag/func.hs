
{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude hiding (width, height, interval)
import Diagrams.Backend.SVG.CmdLine
import Graphics.Diagrams.FunctionGraphs


main :: IO ()
main = defaultMain $ displayFun (-16,-10) (16,10) sin