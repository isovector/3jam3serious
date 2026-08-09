{-# LANGUAGE ViewPatterns #-}

module Jam3Serious.Geometry where

import Debug.Trace
import Foreign.C.Types        (CInt)
import qualified SDL.Raw.Primitive
import SDL.Internal.Types     (Renderer(..))
import Linear.V4
import Data.Word
import Jam3Serious.Types
import SDL qualified
import SDL.Primitive


pointInCapsule :: (Num a, Ord a) => V3 a -> Capsule a -> Bool
pointInCapsule (V3 x y z) (Capsule t b r (V3 cx cy cz)) = do
  let top = cz + t
      bot = cz - b
      dx = (cx - x)
      dy = (cy - y)
      dbz = (cz - b - z)
      dtz = (cz + t - z)
  and
    [ -- Above bottom and below top of the capsule?
      bot - r <= z
    , z <= top + r
    , -- Within the cylinder?
      dx * dx + dy * dy <= r * r
    , -- In the top or bottom hemisphere?
      z < bot ==>
        dx * dx + dy * dy + dbz * dbz <= r * r
    , top < z ==>
        dx * dx + dy * dy + dtz * dtz <= r * r
    ]


capsuleInCapsule :: (Num a, Ord a) => Capsule a -> Capsule a -> Bool
capsuleInCapsule
    (Capsule t1 b1 r1 (V3 x1 y1 z1))
    (Capsule t2 b2 r2 (V3 x2 y2 z2)) = do
  let top1 = z1 + t1
      bot1 = z1 - b1
      top2 = z2 + t2
      bot2 = z2 - b2
      dx = x1 - x2
      dy = y1 - y2
      gap = max 0 $ max (bot2 - top1) (bot1 - top2)
      rsum = r1 + r2
  dx * dx + dy * dy + gap * gap <= rsum * rsum


(==>) :: Bool -> Bool -> Bool
x ==> y = not x || y
infix 0 ==>


v3ToV2 :: V3 Double -> V2 Double
v3ToV2 (V3 x y z) = V2 x ((y / 2) - z)

fillCircle' :: Renderer -> Pos -> Radius -> Color -> IO CInt
fillCircle' (Renderer p) (V2 x y) rad (V4 r g b a) =
  SDL.Raw.Primitive.filledCircle
    p (traceShowId $ fromIntegral x) (traceShowId $ fromIntegral y) (fromIntegral rad) r g b 255

drawCapsule :: Capsule Double -> V4 Word8 -> SDL.Renderer -> IO ()
drawCapsule (Capsule t b (round -> r) xyz) color renderer = do
  let top = fmap round $ v3ToV2 $ xyz + V3 0 0 t
      bot = fmap round $ v3ToV2 $ xyz - V3 0 0 b
  pixel renderer (fmap round $ v3ToV2 xyz) color
  line renderer (top - V2 r 0) (bot - V2 r 0) color
  line renderer (top + V2 r 0) (bot + V2 r 0) color
  arc renderer top r 180 0 color
  arc renderer bot r 0 180 color

