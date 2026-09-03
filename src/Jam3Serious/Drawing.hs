{-# LANGUAGE ViewPatterns #-}

module Jam3Serious.Drawing
  ( module Jam3Serious.Drawing
  , ToScreen(..)
  , getCamera
  ) where

import Control.Arrow
import Control.Lens
import Data.Atlas
import Data.Foldable
import Data.Int
import Data.Map.Strict qualified as M
import Data.Word
import FRP.SFGlobal
import Foreign.C.Types (CInt)
import GHC.Exts (fromList)
import Jam3Serious.Audio
import Jam3Serious.Geometry
import Jam3Serious.Objects.Camera
import Jam3Serious.Types
import Linear.V4
import SDL qualified as SDL
import SDL.Primitive
import Sound.ALUT qualified as ALUT


drawCapsule :: ToScreen -> Capsule Double -> V4 Word8 -> DrawDepth -> Output
drawCapsule cam (Capsule t b r xyz) color = raw $ \renderer _ _ -> do
  let (fmap round -> top, _, st) = toScreen cam $ xyz + V3 0 0 t
      (fmap round -> bot, _, sb) = toScreen cam $ xyz - V3 0 0 b
      (fmap round -> flr, _, sf) = toScreen cam $ xyz & _z .~ 0
      rt = round $ st * r
      rb = round $ sb * r
  fillCircle renderer flr (round $ sf * r) $ V4 0 0 0 128
  pixel renderer (fmap round $ screenPos $ toScreen cam xyz) color
  line renderer (top - V2 rt 0) (bot - V2 rb 0) color
  line renderer (top + V2 rt 0) (bot + V2 rb 0) color
  fillPie renderer top rt 180 0 color
  fillPie renderer bot rb 0 180 color
  horizontalLine renderer bot rb color


billboard :: ToScreen -> Rect3 Double -> V4 Word8 -> DrawDepth -> Output
billboard cam r color = raw $ \renderer _ _ -> do
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
    -> ToScreen
    -> V3 Double
    -> V2 Bool
    -> DrawDepth
    -> Output
drawAnimation (Animation mkAtlas key frameno) cam pos flips = raw $ \renderer gfx _ -> do
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


drawText
    :: (Gfx -> Atlas)
    -> V2 CInt
    -> Int
    -> Output
drawText mkAtlas (V2 x y) (show -> n) =
  flip raw DDGUI $ \renderer gfx _ -> do
    let atlas = mkAtlas gfx
        width = rectWidth $ fst $ (getAtlas atlas M.! "BigLED") !! 0
    for_ (zip [0..] n) $ \(i, d) -> do
      let rect = fst $ (getAtlas atlas M.! "BigLED") !! read [d]
      SDL.copy
        renderer
        (atlasTexture atlas)
        (Just rect)
        (Just $ setRectXY (V2 (x + (width + 2) * i) y) 1 rect)


rectWidth :: SDL.Rectangle a -> a
rectWidth (SDL.Rectangle _ (V2 w _)) = w


setRectXY
    :: Integral a
    => V2 a
    -> Double
    -> SDL.Rectangle a
    -> SDL.Rectangle a
setRectXY xy dsz (SDL.Rectangle _ sz)
  = SDL.Rectangle (SDL.P xy) $ fmap (round . (* dsz) . fromIntegral) sz


playSound
  :: (Soundbank -> Source)
  -> Output
playSound f = flip raw DDGUI $ \_ _ audio -> do
  let src = f audio
  ALUT.stop [src]
  ALUT.play [src]

