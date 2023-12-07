{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Painter where

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

flipV :: Painter -> Painter
flipV = transformPainter (0,1) (1,1) (0,0)

flipH :: Painter -> Painter
flipH = transformPainter (1,0) (0,0) (1,1)

rotL :: Painter -> Painter
rotL = transformPainter (1,0) (1,1) (0,0)

beside :: Float -> Float -> Painter -> Painter -> Painter
beside m n p q = transformPainter (0,0) (r,0) (0,1) p
              <> transformPainter (r,0) (1,0) (r,1) q
    where
        r = m / (m + n)

above :: Float -> Float -> Painter -> Painter -> Painter
above m n p q = transformPainter (0,r) (1,r) (0,1) p
             <> transformPainter (0,0) (1,0) (0,r) q
    where
        r = n / (m + n)

infixr 4 <->
infixr 3 </>

(<->), (</>) :: Painter -> Painter -> Painter
p <-> q = beside 1 1 p q
p </> q = above 1 1 p q

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

transformPainter :: G.Vector -> G.Vector -> G.Vector -> Painter -> Painter
transformPainter o e0 e1 painter frame
    = painter frame'
    where
        m = frameCoordMap frame
        frame' = frame 
               { origin = m o
               , edge0  = m e0 G.- m o
               , edge1  = m e1 G.- m o
               }
