{-# LANGUAGE ViewPatterns #-}

module Jam3Serious.Player where

import Data.Map qualified as M
import Data.Monoid
import Jam3Serious.Ball
import Jam3Serious.Mail
import Jam3Serious.Geometry
import Jam3Serious.Prelude
import Jam3Serious.Router (object)


onPress :: Num a => Scancode -> a -> Input -> a
onPress field a = flip i_keyboard field >>> arr (bool 0 a)


arrows :: Num a => Input -> V2 a
arrows =
  sum
    [ onPress ScancodeW $ V2 0    (-1)
    , onPress ScancodeS $ V2 0    1
    , onPress ScancodeA $ V2 (-1) 0
    , onPress ScancodeD $ V2 1    0
    ]

data Controller = Controller
  { c_dir :: V2 Double
  , c_shoot :: Event ()
  , c_run :: Bool
  }

inputToController :: SF ObjInput Controller
inputToController = proc (oi_input -> i) -> do
  shoot <- edge -< i_keyboard i ScancodeSpace
  returnA -< Controller
    { c_dir = arrows i
    , c_shoot = shoot
    , c_run = i_keyboard i ScancodeLShift
    }


data PlayerState = PlayerState
  { ps_pos :: V3 Double
  , ps_color :: V4 Word8
  , ps_playable :: Bool
  }
  deriving Generic

instance ToObjState PlayerState where
  toObjState ps = ObjState
    { os_pos = Just $ ps_pos ps
    , os_collision = Just $ playerCapsule $ ps_pos ps
    }

playerCapsule :: V3 Double -> Capsule Double
playerCapsule = Capsule 2 0 0.25


player :: Obj PlayerState
player = proc (oi, ps) -> do
  ctrl <-
    case ps_playable ps of
      True -> inputToController -< oi
      False -> do
        t <- time -< ()
        returnA -< Controller
          { c_dir = pure $ cos (t * 10)
          , c_shoot = noEvent
          , c_run = False
          }

  pickup <- onMail @PickMeUp -< oi
  let pass = c_shoot ctrl
      teammate = findTeammate oi

  returnA -<
    ( mempty
        { oo_output = raw $ \r -> do
            drawCapsule
              (playerCapsule $ ps_pos ps)
              (ps_color ps)
              r
        , oo_outbox =
            on pickup $ respond PickedUp
        , oo_commands =
            on pass $ const $ pure $
              Spawn Ball
                $ object
                    (ballState (ps_pos ps + V3 0 0 1.5) (maybe 0 (subtract $ ps_pos ps) (os_pos teammate)) $ Passing $ oi_me oi)
                    ball
        }
    , ps
        & #ps_pos +~ (0 & _xy .~ c_dir ctrl ) ^* (bool 3 6 (c_run ctrl) * i_dt (oi_input oi))
        & #ps_color %~ appEndo (on pickup $ const $ Endo $ const $ V4 0 128 255 255)
    )


findTeammate :: ObjInput -> ObjState
findTeammate oi = fromMaybe (error "no teammate?") $ do
  Player team pnum <- pure $ oi_me oi
  M.lookup (Player team $ otherPlayerNum pnum) (oi_everyone oi)

