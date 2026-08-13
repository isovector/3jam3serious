{-# LANGUAGE ViewPatterns #-}

module Jam3Serious.Drawing where

import Data.Int
import Control.Lens
import GHC.Exts (fromList)
import Data.Word
import Jam3Serious.Camera
import Jam3Serious.Types
import Linear.V4
import SDL.Primitive


v3ToV2 :: V3 Double -> V2 Double
v3ToV2 = fst . toScreen


drawCapsule :: Capsule Double -> V4 Word8 -> Output
drawCapsule (Capsule t b r xyz) color = raw $ \renderer -> do
  let (fmap round -> top, st) = toScreen $ xyz + V3 0 0 t
      (fmap round -> bot, sb) = toScreen $ xyz - V3 0 0 b
      (fmap round -> flr, sf) = toScreen $ xyz & _z .~ 0
      rt = round $ st * r
      rb = round $ sb * r
  fillCircle renderer flr (round $ sf * r) $ V4 0 0 0 255
  pixel renderer (fmap round $ v3ToV2 xyz) color
  line renderer (top - V2 rt 0) (bot - V2 rb 0) color
  line renderer (top + V2 rt 0) (bot + V2 rb 0) color
  fillPie renderer top rt 180 0 color
  fillPie renderer bot rb 0 180 color
  horizontalLine renderer bot rb color


billboard :: V3 Double -> V3 Double -> V3 Double -> V4 Word8 -> Output
billboard c e1 e2 color = raw $ \renderer -> do
  let tl = fst $ toScreen $ c - e1 - e2
      tr = fst $ toScreen $ c - e1 + e2
      br = fst $ toScreen $ c + e1 + e2
      bl = fst $ toScreen $ c + e1 - e2
      poly = fmap (fmap $ round @_ @Int16) [tl, tr, br, bl]
  fillPolygon
    renderer
    (fromList $ fmap (view _x) poly)
    (fromList $ fmap (view _y) poly)
    color

