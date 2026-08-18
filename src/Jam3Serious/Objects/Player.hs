{-# LANGUAGE ViewPatterns #-}

module Jam3Serious.Objects.Player where

import Data.Bezier
import Data.List (sortOn)
import Data.Map qualified as M
import Jam3Serious.Drawing
import Jam3Serious.Mail
import Jam3Serious.Objects.Ball
import Jam3Serious.Objects.Basket
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
  , c_pass :: Event ()
  , c_run :: Bool
  }

inputToController :: SF ObjInput Controller
inputToController = proc (oi_input -> i) -> do
  shoot <- edge -< i_keyboard i ScancodeSpace
  pass <- edge -< i_keyboard i ScancodeF
  returnA -< Controller
    { c_dir = arrows i
    , c_shoot = shoot
    , c_pass = pass
    , c_run = i_keyboard i ScancodeLShift
    }

data PlayerState = PlayerState
  { ps_pos :: V3 Double
  , ps_playable :: Bool
  , ps_hasBall :: Bool
  }
  deriving Generic

instance ToObjState PlayerState where
  toObjState ps = ObjState
    { os_pos = Just $ ps_pos ps
    , os_collision = Just $ playerCapsule $ ps_pos ps
    }

playerCapsule :: V3 Double -> Capsule Double
playerCapsule = Capsule 2 0.1 0.25

teamColor :: Name -> V4 Word8
teamColor (Player T1 _) = V4 255 0 0 255
teamColor (Player T2 _) = V4 0 0 255 255
teamColor _ = V4 0 0 0 255


data PlayerAction
  = WalkTo (V2 Double)
  -- | ShovedFrom (V2 Double)

walkSpeed :: Num a => a
walkSpeed = 3


runSpeed :: Num a => a
runSpeed = 6

player :: Obj PlayerState
player = foreverSwont $ do
  swont runPlayer >>= \case
   WalkTo goal -> swont $ gotoPlayer goal


gotoPlayer :: V2 Double -> ObjE PlayerState ()
gotoPlayer goal = proc (oi, ps) -> do
  let pos = ps_pos ps ^. _xy
  let dist = distance pos goal
  arrived <- edge -< dist <= 0.01
  rendered <- renderPlayer -< (oi, ps)
  returnA -<
    ( ( mempty { oo_output = rendered }
      , ps & #ps_pos ._xy +~ normalize (goal - pos) ^* min dist (walkSpeed * i_dt (oi_input oi))
      )
    , arrived
    )



onceUntil :: SF (Event a, Event clear) (Event a)
onceUntil = proc (ea, eclear) -> do
  rec
    let ea' = gate ea canSend
    canSend <- dHold True -< asum [True <$ eclear, False <$ ea']
  returnA -< ea'



runPlayer :: ObjE PlayerState PlayerAction
runPlayer = proc (oi, ps) -> do
  ctrl <-
    case ps_playable ps of
      True -> inputToController -< oi
      False -> do
        c' <- inputToController -< oi
        returnA -< c'
          { c_dir = 0
          , c_run = False
          }

  couldPickup <- onMail @PickMeUp -< oi

  let pass = PassTo (V3 0 0 0) <$ gate (c_pass ctrl) (ps_hasBall ps)
      shoot = ShootAt (V3 (-5) 0 4) <$ gate (c_shoot ctrl) (ps_hasBall ps)

  afterwards <- delayEvent 0.5 -< pass <|> shoot
  pickup <- onceUntil -< (couldPickup, afterwards)


  rendered <- renderPlayer -< (oi, ps)

  returnA -< (, asum [ ]) $
    ( mempty
        { oo_output = rendered
        , oo_outbox = mconcat
            [ on pickup $ respond PickedUp
            , on (shoot <|> pass) $ send Ball
            ]
        , oo_commands =
            on (shoot <|> pass) $ const $ pure $
              Spawn Ball
                $ object
                    (ballState
                      (ps_pos ps + V3 0 0 1.5)
                      0
                      -- (maybe 0 (subtract $ ps_pos ps) (os_pos teammate))
                    )
                    ball
        }
    , ps
        & #ps_pos +~ (0 & _xy .~ c_dir ctrl) ^* (bool walkSpeed runSpeed (c_run ctrl) * i_dt (oi_input oi))
        & #ps_hasBall %~ appEndo (
              mconcat
                [ on pickup (const $ Endo $ const True)
                , on pass (const $ Endo $ const False)
                , on shoot (const $ Endo $ const False)
                ])
    )


renderPlayer :: SF (ObjInput, PlayerState) Output
renderPlayer = proc (oi, ps) -> do
  ballZ <- fmap (abs . cos . (* 8)) time -< ()
  let depth = DDDepth $ view _y $ ps_pos ps
  returnA -< mconcat
    [ drawCapsule oi
        (playerCapsule $ ps_pos ps)
        (teamColor $ oi_me oi)
        depth
    , flip (bool mempty) (ps_hasBall ps) $
        drawCapsule oi
          (ballCapsule $ (ps_pos ps + V3 0.25 0 0) & _z .~ ballZ)
          (V4 255 128 0 255)
          depth
    ]


nearestTeammate :: ObjInput -> ObjState
nearestTeammate oi = fromMaybe (error "no teammate?") $ do
  me@(Player meteam _) <- pure $ oi_me oi
  mepos <- os_pos =<< M.lookup me (oi_everyone oi)
  fmap snd $ listToMaybe $ sortOn fst $ do
    (name@(Player team _), os) <- M.toList $ oi_everyone oi
    guard $ team == meteam && name /= me
    pos <- maybeToList $  os_pos os
    pure (qd mepos pos, os)


mkShootBezier :: ObjInput -> Team -> Bezier Double (V3 Double)
mkShootBezier oi t = bezier
  [ (fromMaybe (error $ "no pos for me " <> show (oi_me oi)) $
      os_pos =<< M.lookup (oi_me oi) (oi_everyone oi)) + V3 0 0 shootHeight
  , netPos oi t + V3 0 0 3
  , netPos oi t
  ]

