{-# OPTIONS_GHC -Wno-orphans #-}

module Jam3Serious.Basket where

import Jam3Serious.Prelude
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
        { oo_output = raw $ \r -> do
            ellipse r
              (fmap round $ fst $ toScreen pos) 40 10 $ V4 255 0 0 255
        }
    , pos
    )

