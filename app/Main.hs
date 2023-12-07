module Main (main) where

import Graphics.Gloss
import Painter
import Wave

main :: IO ()
main = display (InWindow "square limit" (640, 480) (100, 100)) white (p1 frame0)

frame0 :: Frame
frame0 = Frame (0, 0) (100, 0) (0, 100)

p0 :: Painter
p0 = flipV wave <-> wave

p1 :: Painter
p1 = p0 </> p0
