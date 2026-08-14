{-# OPTIONS_GHC -Wno-orphans #-}

module Jam3Serious.Objects.Basket where

import Data.Map qualified as M
import Jam3Serious.Prelude
import Jam3Serious.Drawing
import Jam3Serious.Camera
import SDL.Primitive


instance ToObjState (V3 Double) where
  toObjState p = ObjState
    { os_pos = Just p
    , os_collision = Nothing
    }


basketWidth, basketHeight :: Num a => a
basketWidth = 40
basketHeight = 20


basket :: Obj (V3 Double)
basket = proc (_, pos) -> do
  returnA -<
    ( mempty
        { oo_output =
            mconcat
              [ billboard (pos + V3 (-0.5) 0 0) (V3 0 (1.83 / 2) 0) (V3 0 0 (1.07 / 2)) $ V4 128 128 0 255
              , billboard (pos + V3 (-0.5) 0 (-0.2)) (V3 0 (0.61 / 2) 0) (V3 0 0 (0.457 / 2)) $ V4 255 255 0 255
              , raw $ \r -> do
                  ellipse r
                    (fmap round $ fst $ toScreen pos) 40 10 $ V4 255 0 0 255
              ]
        }
    , pos
    )


netPos :: ObjInput -> Team -> V3 Double
netPos oi t =
  fromMaybe (error $ "no net for team " <> show t) $
    os_pos =<< M.lookup (Basket t) (oi_everyone oi)

