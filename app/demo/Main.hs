module Main (main) where

import Graphics.Gloss
import Painter
import SquareLimit
import Wave
import Fish

main :: IO ()
main = display FullScreen white (square 2 frame0)

frame0 :: Frame
-- frame0 = Frame (-200, -200) (200, -200) (-200, 200)
frame0 = Frame (-400,-400) (800,0) (0,800)
p0 :: Painter
p0 = flipV wave <-> wave

p1 :: Painter
p1 = p0 </> p0

p2 :: Painter
p2 = squareLimit 8 wave

