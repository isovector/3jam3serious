{-# LANGUAGE ViewPatterns #-}

module Jam3Serious.Drawing where

import Control.Lens
import Data.Word
import Jam3Serious.Camera
import Jam3Serious.Types
import Linear.V4
import SDL qualified
import SDL.Primitive


v3ToV2 :: V3 Double -> V2 Double
v3ToV2 = fst . toScreen


drawCapsule :: Capsule Double -> V4 Word8 -> SDL.Renderer -> IO ()
drawCapsule (Capsule t b r xyz) color renderer = do
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

