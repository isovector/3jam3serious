{-# LANGUAGE ViewPatterns #-}

module Jam3Serious.Drawing where

import Control.Lens
import Data.Int
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Word
import GHC.Exts (fromList)
import Jam3Serious.Geometry
import Jam3Serious.Objects.Camera
import Jam3Serious.Types
import Linear.V4
import SDL.Primitive


getCamera :: ObjInput -> V3 Double
getCamera oi =
  fromMaybe 0 $ os_pos =<< M.lookup Camera (oi_everyone oi)


drawCapsule :: ObjInput -> Capsule Double -> V4 Word8 -> DrawDepth -> Output
drawCapsule oi (Capsule t b r xyz) color = raw $ \renderer -> do
  let cam = getCamera oi
      (fmap round -> top, st) = toScreen cam $ xyz + V3 0 0 t
      (fmap round -> bot, sb) = toScreen cam $ xyz - V3 0 0 b
      (fmap round -> flr, sf) = toScreen cam $ xyz & _z .~ 0
      rt = round $ st * r
      rb = round $ sb * r
  fillCircle renderer flr (round $ sf * r) $ V4 0 0 0 255
  pixel renderer (fmap round $ fst $ toScreen cam xyz) color
  line renderer (top - V2 rt 0) (bot - V2 rb 0) color
  line renderer (top + V2 rt 0) (bot + V2 rb 0) color
  fillPie renderer top rt 180 0 color
  fillPie renderer bot rb 0 180 color
  horizontalLine renderer bot rb color


billboard :: ObjInput -> Rect3 Double -> V4 Word8 -> DrawDepth -> Output
billboard oi r color = raw $ \renderer -> do
  let cam = getCamera oi
      V4 tl tr br bl = fmap (fst . toScreen cam) $ rectCorners r
      poly = fmap (fmap $ round @_ @Int16) [tl, tr, br, bl]
      c = fmap round $ fst $ toScreen cam $ r3_center r
      c' = fmap round $ fst $ toScreen cam $ r3_center r + rectNormal r
  fillPolygon
    renderer
    (fromList $ fmap (view _x) poly)
    (fromList $ fmap (view _y) poly)
    color
  line renderer c c' (V4 255 0 0 92)

