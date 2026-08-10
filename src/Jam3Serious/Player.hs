{-# LANGUAGE ViewPatterns #-}

module Jam3Serious.Player where

import Linear.Metric (qd)
import Data.List (sortOn)
import Data.Map qualified as M
import Jam3Serious.Ball
import Jam3Serious.Mail
import Jam3Serious.Drawing
import Jam3Serious.Prelude


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

data Pattern
  = BackAndForth
  | Circle

data PlayerState = PlayerState
  { ps_pos :: V3 Double
  , ps_playable :: Bool
  , ps_pattern :: Pattern
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
        c' <- inputToController -< oi
        t <- time -< ()
        returnA -< c'
          { c_dir =
              case ps_pattern ps of
                BackAndForth -> V2 (cos (t * 5)) 0
                Circle -> V2 (cos (-t * 10)) (sin (t * 10))
          , c_run = False
          }

  pickup <- onMail @PickMeUp -< oi
  let pass = c_shoot ctrl
      teammate = nearestTeammate oi

  hasBall <- iPre False <<< hold False -< asum
    [ True <$ pickup
    , False <$ pass
    ]

  ballZ <- fmap (abs . cos . (* 8)) time -< ()

  returnA -<
    ( mempty
        { oo_output = raw $ \r -> do
            drawCapsule
              (playerCapsule $ ps_pos ps)
              (case hasBall of
                 True -> V4 0 128 255 255
                 False -> V4 0 0 0 255
              )
              r
            when hasBall $ do
              drawCapsule
                (ballCapsule $ (ps_pos ps + V3 0.25 0 0) & _z .~ ballZ)
                (V4 255 128 0 255)
                r
        , oo_outbox =
            on pickup $ respond PickedUp
        , oo_commands =
            on (gate pass hasBall) $ const $ pure $
              Spawn Ball
                $ object
                    (ballState (ps_pos ps + V3 0 0 1.5) (maybe 0 (subtract $ ps_pos ps) (os_pos teammate)) $ Passing $ oi_me oi)
                    ball
        }
    , ps
        & #ps_pos +~ (0 & _xy .~ c_dir ctrl ) ^* (bool 3 6 (c_run ctrl) * i_dt (oi_input oi))
    )


nearestTeammate :: ObjInput -> ObjState
nearestTeammate oi = fromMaybe (error "no teammate?") $ do
  me@(Player meteam _) <- pure $ oi_me oi
  mepos <- os_pos =<< M.lookup me (oi_everyone oi)
  fmap snd $ listToMaybe $ sortOn fst $ do
    (name@(Player team _), os) <- M.toList $ oi_everyone oi
    guard $ team == meteam && name /= me
    pos <- maybeToList $  os_pos os
    pure (qd mepos pos, os)

