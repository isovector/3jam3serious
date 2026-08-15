{-# OPTIONS_GHC -Wno-orphans #-}

module Jam3Serious.Objects.Basket where

import Data.Map qualified as M
import Jam3Serious.Prelude
import Jam3Serious.Drawing
import Jam3Serious.Objects.Camera
import SDL.Primitive


instance ToObjState (V3 Double) where
  toObjState p = ObjState
    { os_pos = Just p
    , os_collision = Nothing
    }


basketWidth, basketHeight :: Num a => a
basketWidth = 40
basketHeight = 20


basketRect :: V3 Double -> V3 Double -> Rect3 Double
basketRect normal pos = do
  let n = normalize normal
      up = V3 0 0 1
      uDir = normalize $ cross up n
      vDir = normalize $ cross n uDir
      u = uDir ^* 1.83 / 2
      v = vDir ^* 1.07 / 2
  Rect3 pos u v


basket :: V3 Double -> Obj (V3 Double)
basket normal = proc (oi, pos) -> do
  let bb = basketRect normal $ pos - normal * 0.5
  returnA -<
    ( mempty
        { oo_output =
            mconcat
              [ billboard oi bb $ V4 128 128 0 255
              , raw $ \r -> do
                  ellipse r
                    (fmap round $ fst $ toScreen (getCamera oi) pos) 40 10 $ V4 255 0 0 255
              ]
        }
    , pos
    )


netPos :: ObjInput -> Team -> V3 Double
netPos oi t =
  fromMaybe (error $ "no net for team " <> show t) $
    os_pos =<< M.lookup (Basket t) (oi_everyone oi)

