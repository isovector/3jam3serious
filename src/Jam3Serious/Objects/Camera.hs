module Jam3Serious.Objects.Camera where

import Data.Map qualified as M
import Linear.Matrix
import Linear.Projection
import Linear.V2
import Linear.V3
import Linear.V4
import Jam3Serious.Prelude hiding (identity)
import Jam3Serious.Mail

data CameraState = CameraState
  { cs_pos :: V3 Double
  , cs_deadzone :: Double
  , cs_speed :: Double
  , cs_focus :: Name
  }
  deriving Generic

instance ToObjState CameraState where
  toObjState cs = ObjState
    { os_pos = Just $ cs_pos cs
    , os_collision = Nothing
    }

data Refocus = RefocusOnMe

camera :: Obj CameraState
camera = proc (oi, cs) -> do
  refocus <- onMail @Refocus -< oi

  let focus = fromMaybe 0
            $ os_pos =<< M.lookup (cs_focus cs) (oi_everyone oi)
      diff = norm $ focus - cs_pos cs
  returnA -<
    ( mempty
    , cs
        & #cs_focus %~ appEndo (on refocus $ Endo . const . from)
        & #cs_pos %~ \pos ->
          case qd (fst $ toScreen pos focus) (fst $ toScreen pos pos) > cs_deadzone cs of
            False -> pos
            True -> pos + min diff (cs_speed cs * i_dt (oi_input oi)) *^ normalize (focus - pos)

    )


windowWidth, windowHeight :: Num a => a
windowWidth  = 800
windowHeight = 600


projection :: M44 Double
projection =
  perspective
    (15 * pi / 180)
    (windowWidth / windowHeight)
    5
    35


toScreen :: V3 Double -> V3 Double -> (V2 Double, Double)
toScreen camPos (V3 wx wy wz) =
    ( V2 ( sx * windowWidth  / (2 * sw) + windowWidth / 2)
         (-sy * windowHeight / (2 * sw) + windowHeight / 2)
    , (projection ^. _y._y) * windowHeight / (2 * sw)
    )
  where
    cam = lookAt (V3 0 30 20) (V3 0 0 0) $ V3 0 0 1
    pos = identity & translation .~ (camPos & _yz %~ negate)
    m = cam !*! pos
    V4 sx sy _ sw = projection !*! m !* V4 (-wx) wy wz 1


toScreenNormalized :: V3 Double -> V3 Double -> V2 Double
toScreenNormalized cam
  = (* V2 (2 / windowWidth) (2 / windowHeight))
  . subtract (V2 (windowWidth / 2) (windowHeight / 2))
  . fst
  . toScreen cam

