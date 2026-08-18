{-# OPTIONS_GHC -Wno-orphans #-}

module Jam3Serious.Objects.Court where

import Jam3Serious.Prelude
import Jam3Serious.Drawing
import Jam3Serious.Objects.Basket


courtGeom :: [(Rect3 Double, V4 Word8)]
courtGeom =
  [ -- floor
    (Rect3 0 (V3 w 0 0) (V3 0 d 0), V4 92 92 92 128)
  , -- ceiling
    (Rect3 (V3 0 0 height) (V3 0 d 0) (V3 w 0 0) , V4 0 0 0 0)
  , -- back wall
    (Rect3 (V3 0 (-d) h) (V3 0 0 h) (V3 w 0 0), V4 0 0 0 92)
  , -- left wall
    (Rect3 (V3 (-w) 0 h) (V3 0 d 0) (V3 0 0 h) , V4 0 0 0 92)
  , -- right wall
    (Rect3 (V3 w 0 h) (V3 0 0 h) (V3 0 d 0), V4 0 0 0 92)
  , -- front wall
    (Rect3 (V3 0 d h)  (V3 w 0 0) (V3 0 0 h), V4 0 0 0 0)

    -- left basket
  , (basketRect (V3 1 0 0) (V3 (-5.5) 0 4), V4 255 0 0 255)
  , (basketRect (V3 (-1) 0 0) (V3 (-5.55) 0 4), V4 255 0 0 255)
    -- right basket
  , (basketRect (V3 (-1) 0 0) (V3 (5.5) 0 4), V4 255 0 0 255)
  , (basketRect (V3 1 0 0) (V3 5.55 0 4), V4 255 0 0 255)
  ]
  where
    width = 28.65
    depth = 15.24
    height = 10
    w = width / 2
    d = depth / 2
    h = height / 2


instance ToObjState () where
  toObjState () = ObjState Nothing Nothing


court :: Obj ()
court = arr $ first $ \oi ->
  mempty { oo_output = foldMap (\(r3, c) -> billboard oi r3 c DDCourt) courtGeom }

