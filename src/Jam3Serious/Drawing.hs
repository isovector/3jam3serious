{-# LANGUAGE ViewPatterns #-}

module Jam3Serious.Drawing
  ( module Jam3Serious.Drawing
  , CamPos(..)
  , getCamera
  ) where

import Control.Arrow
import Control.Lens
import Data.Atlas
import Data.Int
import Data.Map.Strict qualified as M
import Data.Word
import FRP.SFGlobal
import GHC.Exts (fromList)
import Jam3Serious.Geometry
import Jam3Serious.Objects.Camera
import Jam3Serious.Types
import Linear.V4
import SDL qualified as SDL
import SDL.Primitive


drawCapsule :: CamPos -> Capsule Double -> V4 Word8 -> DrawDepth -> Output
drawCapsule cam (Capsule t b r xyz) color = raw $ \renderer _ -> do
  let (fmap round -> top, _, st) = toScreen cam $ xyz + V3 0 0 t
      (fmap round -> bot, _, sb) = toScreen cam $ xyz - V3 0 0 b
      (fmap round -> flr, _, sf) = toScreen cam $ xyz & _z .~ 0
      rt = round $ st * r
      rb = round $ sb * r
  fillCircle renderer flr (round $ sf * r) $ V4 0 0 0 255
  pixel renderer (fmap round $ screenPos $ toScreen cam xyz) color
  line renderer (top - V2 rt 0) (bot - V2 rb 0) color
  line renderer (top + V2 rt 0) (bot + V2 rb 0) color
  fillPie renderer top rt 180 0 color
  fillPie renderer bot rb 0 180 color
  horizontalLine renderer bot rb color


billboard :: CamPos -> Rect3 Double -> V4 Word8 -> DrawDepth -> Output
billboard cam r color = raw $ \renderer _ -> do
  let V4 tl tr br bl = fmap (screenPos . toScreen cam) $ rectCorners r
      poly = fmap (fmap $ round @_ @Int16) [tl, tr, br, bl]
      c = fmap round $ screenPos $ toScreen cam $ r3_center r
      c' = fmap round $ screenPos $ toScreen cam $ r3_center r + rectNormal r
  fillPolygon
    renderer
    (fromList $ fmap (view _x) poly)
    (fromList $ fmap (view _y) poly)
    color
  line renderer c c' (V4 255 0 0 92)

data Anim = Anim
  { animKey :: String
  , animRate :: Time
  }
  deriving stock (Eq, Ord, Show)

data Animation = Animation
  { a_mkAtlas :: Gfx -> Atlas
  , a_key :: String
  , a_frameno :: Int
  }


animate :: (Gfx -> Atlas) -> SF Anim Animation
animate mkAtlas = proc anim -> do
  t <- time -< ()
  let frameno = floor $ t / animRate anim
  returnA -< Animation mkAtlas (animKey anim) frameno


drawAnimation
    :: Animation
    -> CamPos
    -> V3 Double
    -> V2 Bool
    -> DrawDepth
    -> Output
drawAnimation (Animation mkAtlas key frameno) cam pos flips = raw $ \renderer gfx -> do
    let atlas = mkAtlas gfx
        (fmap round -> spos, sz, _) = toScreen cam pos
        frames = getAtlas atlas M.! key
        (rect, origin) = frames !! (mod frameno $ length frames)
    SDL.copyEx
      renderer
      (atlasTexture atlas)
      (Just $ rect)
      (Just $ setRectXY (spos - fmap round (fmap fromIntegral origin SDL.^* sz)) sz rect)
      0
      Nothing
      flips


setRectXY
    :: Integral a
    => V2 a
    -> Double
    -> SDL.Rectangle a
    -> SDL.Rectangle a
setRectXY xy dsz (SDL.Rectangle _ sz)
  = SDL.Rectangle (SDL.P xy) $ fmap (round . (* dsz) . fromIntegral) sz

