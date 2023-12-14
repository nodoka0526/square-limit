{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
module SquareLimit where

import Painter

squareLimit :: Int -> Painter -> Painter
squareLimit n p = half </> flipV half
    where
        half = flipH quarter <-> quarter
        quarter = cornerSplit (n-1) p

cornerSplit :: Int -> Painter -> Painter
cornerSplit 0 _ = blank
cornerSplit n p = (upLeft </> p) <-> (upRight </> bottomRight)
    where
        upLeft = upSplit (n-1) p
        upRight = cornerSplit (n-1) p
        bottomRight = rightSplit (n-1) p

upSplit :: Int -> Painter -> Painter
upSplit 0 _ = blank
upSplit n p = small <-> small </> p
    where
        small = upSplit (n-1) p

rightSplit :: Int -> Painter -> Painter
rightSplit 0 _ = blank
rightSplit n p = p <-> (small </> small)
    where
        small = rightSplit (n-1) p

