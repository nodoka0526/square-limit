{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
module SquareLimit where

import Graphics.Gloss qualified as G
import Graphics.Gloss.Data.Point.Arithmetic qualified as G

type LineSegment = [G.Vector]
type Figure = [LineSegment]
data Frame = Frame
    { origin :: G.Vector
    , edge0  :: G.Vector
    , edge1  :: G.Vector
    }
type Painter = Frame -> G.Picture

wave :: Painter
wave = segmentsToPainter 20 20
    [[(0,13),(3,8),(6,12),(7,11),(5,0)],[(8,0),(10,6),(12,0)],
     [(15,0),(12,10),(20,3)],[(20,7),(15,13),(12,13),(13,17),(12,20)],
     [(8,20),(7,17),(8,13),(6,13),(3,12),(0,17)]]

drawLine :: G.Path -> G.Picture
drawLine = G.line

segmentsToPainter :: Float -> Float -> Figure -> Painter
segmentsToPainter scale0 scale1 segs frame =
            let toFrame (x,y) = frameCoordMap frame (x/scale0, y/scale1)
                drawSeg seg = drawLine (map toFrame seg)
            in
              mconcat (map drawSeg segs)

frameCoordMap :: Frame -> (G.Vector -> G.Vector)
frameCoordMap frame (x,y) 
    = frame.origin G.+ x G.* frame.edge0 G.+ y G.* frame.edge1

