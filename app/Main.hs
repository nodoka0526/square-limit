module Main (main) where

import Graphics.Gloss
import Painter
import SquareLimit
import Wave

main :: IO ()
main = display (InWindow "square limit" (640, 480) (100, 100)) white (p2 frame0)

frame0 :: Frame
-- frame0 = Frame (-200, -200) (200, -200) (-200, 200)
frame0 = Frame (-200,-200) (400,0) (0,400)
p0 :: Painter
p0 = flipV wave <-> wave

p1 :: Painter
p1 = p0 </> p0

p2 :: Painter
p2 = squareLimit 8 wave

