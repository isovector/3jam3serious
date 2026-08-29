{-# LANGUAGE ViewPatterns #-}

module Jam3Serious.Drawing where

import Control.Lens
import Data.Atlas
import Data.Int
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Word
import FRP.Yampa
import GHC.Exts (fromList)
import Jam3Serious.Geometry
import Jam3Serious.Objects.Camera
import Jam3Serious.Types
import Linear.V4
import SDL.Primitive
import SDL qualified as SDL


getCamera :: ObjInput -> V3 Double
getCamera oi =
  fromMaybe 0 $ os_pos =<< M.lookup Camera (oi_everyone oi)


drawCapsule :: ObjInput -> Capsule Double -> V4 Word8 -> DrawDepth -> Output
drawCapsule oi (Capsule t b r xyz) color = raw $ \renderer _ -> do
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
billboard oi r color = raw $ \renderer _ -> do
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


drawSprite :: (Gfx -> Atlas) -> SF (ObjInput, (String, Time), DrawDepth, V3 Double) Output
drawSprite mkAtlas = proc (oi, (key, dur), depth, pos) -> do
  t <- time -< ()
  let frameno = floor $ t / dur
  returnA -< flip raw depth $ \renderer gfx -> do
    let atlas = mkAtlas gfx
        cam = getCamera oi
        (fmap round -> spos, st) = toScreen cam pos
        frames = getAtlas atlas M.! key
        frame = mod frameno $ length frames
    SDL.copy
      renderer
      (atlasTexture atlas)
      (Just $ frames !! frame)
      (Just $ setRectXY spos (frames !! frame))


-- TODO(sandy): total hack for now
setRectXY :: Num a => V2 a -> SDL.Rectangle a -> SDL.Rectangle a
setRectXY xy (SDL.Rectangle _ sz) = SDL.Rectangle (SDL.P $ xy - V2 20 80) sz

