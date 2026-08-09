module Jam3Serious.Camera where

import Control.Lens ((^.), (.~), (&))
import Linear.Matrix
import Linear.Projection
import Linear.V2
import Linear.V3
import Linear.V4


windowWidth, windowHeight :: Num a => a
windowWidth  = 800
windowHeight = 600


projection :: M44 Double
projection =
  perspective
    (15 * pi / 180)
    (windowWidth / windowHeight)
    5
    35

toScreen :: V3 Double -> (V2 Double, Double)
toScreen (V3 wx wy wz) =
    ( V2 ( sx * windowWidth  / (2 * sw) + windowWidth / 2)
         (-sy * windowHeight / (2 * sw) + windowHeight / 2)
    , (projection ^. _y._y) * windowHeight / (2 * sw)
    )
  where
    cam = lookAt (V3 0 30 20) (V3 0 0 0) $ V3 0 0 1
    pos = identity & translation .~ 0
    m = cam !*! pos
    V4 sx sy _ sw = projection !*! m !* V4 (-wx) wy wz 1

