module Main (main) where

import Graphics.Gloss
import Painter
import SquareLimit
import Wave
import Fish

main :: IO ()
main = display FullScreen white (twinhf wave frame0)

frame0 :: Frame
frame0 = Frame (-400,-400) (800,0) (0,800)

twinhf :: Painter -> Painter
twinhf p = p <-> flipH p
