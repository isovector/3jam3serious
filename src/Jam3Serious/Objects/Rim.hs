{-# OPTIONS_GHC -Wno-orphans #-}

module Jam3Serious.Objects.Rim where

import Jam3Serious.Prelude
import Jam3Serious.Drawing
import Jam3Serious.Objects.Camera
import SDL.Primitive


rim :: Obj (V3 Double)
rim = proc (_, pos) -> do
  cam <- getCamera -< ()
  returnA -<
    ( mempty
        { oo_output =
            flip raw (DDDepth $ view _y pos + 1) $ \r _ _ -> do
              fillEllipse r
                (fmap round $ screenPos $ toScreen cam pos) 2 2 $ V4 0 0 0 255
        }
    , pos
    )

