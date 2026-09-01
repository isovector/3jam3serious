module Jam3Serious.Objects.Camera where

import Data.Map qualified as M
import Linear.Matrix
import Linear.Projection
import Linear.V2
import Linear.V3
import Linear.V4
import Jam3Serious.Prelude
import Jam3Serious.Mail

newtype CamPos = CamPos
  { getCamPos :: V3 Double
  }
  deriving stock (Eq, Ord, Show)


getCamera :: SF a CamPos
getCamera = global >>> arr (\g ->
  CamPos $ fromMaybe 0 $ os_pos =<< M.lookup Camera (g_everyone g))


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
  everyone <- fmap g_everyone global -< ()

  let focus = fromMaybe 0
            $ os_pos =<< M.lookup (cs_focus cs) everyone
      diff = norm $ focus - cs_pos cs
  returnA -<
    ( mempty
    , cs
        & #cs_focus %~ appEndo (on refocus $ Endo . const . from)
        & #cs_pos %~ \pos ->
          case qd (screenPos $ toScreen (CamPos pos) focus) (screenPos $ toScreen (CamPos pos) pos) > cs_deadzone cs of
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


toScreen :: CamPos -> V3 Double -> (V2 Double, Double, Double)
toScreen (CamPos camPos) (V3 wx wy wz) =
    ( V2 ( sx * windowWidth  / (2 * sw) + windowWidth / 2)
         (-sy * windowHeight / (2 * sw) + windowHeight / 2)
    -- Perspective size factor, normalized so that 0 is at the horizon
    -- (infinite depth) and 1 is at the object the camera is centered on
    -- (i.e. whatever sits at camPos), where it isn't foreshortened.
    , refDepth / sw
    -- Pixel-space size factor: how many pixels tall one world-space unit
    -- spans at this depth.
    , (projection ^. _y._y) * windowHeight / (2 * sw)
    )
  where
    camEye = V3 0 30 20
    cam = lookAt camEye (V3 0 0 0) $ V3 0 0 1
    pos = identity & translation .~ (camPos & _yz %~ negate)
    m = cam !*! pos
    -- Eye-space depth of an arbitrary world point.
    depthOf (V3 x y z) = let V4 _ _ _ w = projection !*! m !* V4 (-x) y z 1 in w
    refDepth = depthOf camPos
    V4 sx sy _ sw = projection !*! m !* V4 (-wx) wy wz 1


screenPos :: (V2 Double, Double, Double) -> V2 Double
screenPos (p, _, _) = p


toScreenNormalized :: CamPos -> V3 Double -> V2 Double
toScreenNormalized cam
  = (* V2 (2 / windowWidth) (2 / windowHeight))
  . subtract (V2 (windowWidth / 2) (windowHeight / 2))
  . screenPos
  . toScreen cam

