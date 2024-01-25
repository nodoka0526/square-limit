module Main (main) where

import System.Environment
import Graphics.Gloss
import Painter
import SquareLimit
import Wave
import Fish

main :: IO ()
main = do
    { args <- getArgs
    ; case args of
        []    -> display (screen 0) white (demo 10 frame0)
        a:[]  -> display (screen 0) white (demo (read a) frame0)
        a:s:_ -> display (screen (read s)) white (demo (read a) frame0)
    }

screen :: Int -> Display
screen i = case i of
    0 -> FullScreen
    1 -> InWindow "Square Limit" (1000, 1000) (100,100)
    _ -> InWindow "Square Limit" (1280, 1000) (100,100)

frame0 :: Frame
frame0 = Frame (-400,-400) (800,0) (0,800)

frame1 = Frame (0,0) (400,0) (0,400)        -- 第一象限
frame2 = Frame (-400,0) (400,0) (0,400)     -- 第二象限
frame3 = Frame (-400,-400) (400,0) (0,400)  -- 第三象限
frame4 = Frame (0,-400) (400,0) (0,400)     -- 第四象限

demo :: Int -> Painter
demo i = case i of
    00 -> wave
    01 -> flipH wave
    02 -> flipV wave
    03 -> wave2
    04 -> wave4
    05 -> flippedPairs wave
    06 -> upSplit 4 wave
    07 -> rightSplit 4 wave
    08 -> cornerSplit 4 wave
    09 -> squareLimit 8 wave
    10 -> fish
    11 -> fish4
    12 -> fish3
    _  -> square 4
    where
        wave2 = wave <-> flipV wave
        wave4 = wave2 </> wave2

flippedPairs :: Painter -> Painter
flippedPairs p = q </> flipV q
    where
        q = p <-> flipH p
