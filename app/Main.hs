module Main (main) where

import Graphics.Gloss
import SquareLimit
import SquareLimit (flipV, rotL)

main :: IO ()
main = display (InWindow "square limit" (640, 480) (100, 100)) white ((rotL . rotL) wave frame0)

frame0 :: Frame
frame0 = Frame (0, 0) (100, 0) (0, 100)

