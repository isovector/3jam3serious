{-# LANGUAGE ViewPatterns #-}

module Jam3Serious.Objects.Player where

import Jam3Serious.Geometry
import Data.Bezier
import Data.List (sortOn)
import Data.Map qualified as M
import Jam3Serious.Drawing
import Jam3Serious.Mail
import Jam3Serious.Objects.Ball
import Jam3Serious.Objects.Basket
import Jam3Serious.Objects.Camera
import Jam3Serious.Prelude
import Data.Ord (clamp)
import SDL.Primitive (fillPie, fillCircle)


onPress :: Num a => Scancode -> a -> Input -> a
onPress field a = flip (checkKeyboard . i_keyboard) field >>> arr (bool 0 a)


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
  , c_jump :: Event ()
  , c_shoot :: Event ()
  , c_pass :: Event ()
  , c_run :: Bool
  }

fallingEdge :: SF Bool (Event ())
fallingEdge = edgeBy (\x y -> bool Nothing (Just ()) $ x && not y) True

inputToController :: SF ObjInput Controller
inputToController = proc (oi_input -> i) -> do
  jump <- edge -< (checkKeyboard . i_keyboard) i ScancodeSpace
  shoot <- fallingEdge -< (checkKeyboard . i_keyboard) i ScancodeSpace
  pass <- edge -< (checkKeyboard . i_keyboard) i ScancodeF
  returnA -< Controller
    { c_dir = arrows i
    , c_jump = jump
    , c_shoot = shoot
    , c_pass = pass
    , c_run = (checkKeyboard . i_keyboard) i ScancodeLShift
    }

data PlayerState = PlayerState
  { ps_pos :: V3 Double
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
  = Jump
  -- | ShovedFrom (V2 Double)
  deriving stock (Eq, Ord, Show)

walkSpeed :: Num a => a
walkSpeed = 3


runSpeed :: Num a => a
runSpeed = 6

getState :: SF (Controller, ObjInput, s) r -> Swont (Controller, ObjInput, s) (ObjOutput, s) r
getState f = swont $ proc i@(_, _, s) -> do
  r <- f -< i
  returnA  -< ((mempty, s), Event r)


player :: SF (ObjInput, PlayerState) Controller -> Obj PlayerState
player getCtrls = wrapPlayer getCtrls $ foreverSwont $ do
  swont runPlayer >>= \case
   Jump -> do
     p <- getState $ arr $ ps_pos . view _3
     dir <- getState $ arr $ c_dir  . view _1
     let vel = 0 & _xy .~ dir * walkSpeed

     let jumpHeight = 1.5
     swont $ motionPlayer 1 $ bezier
      [ p
      , p + V3 0 0 (2 * jumpHeight) + vel * 0.5
      , p + vel
      ]


motionPlayer :: Time -> Bezier Double (V3 Double) -> SF (Controller, ObjInput, PlayerState) ((ObjOutput, PlayerState), Event ())
motionPlayer dur bez = proc (_, oi, ps) -> do
  t <- time -< ()
  done <- after dur () -< ()

  rendered <- renderPlayer -< (Jumping, oi, ps)


  returnA -<
    ( ( mempty { oo_output = rendered }
      , ps
          & #ps_pos .~ runBezier bez (t / dur)
      )
    , done
    )


onceUntil :: SF (Event a, Event clear) (Event a)
onceUntil = proc (ea, eclear) -> do
  rec
    let ea' = gate ea canSend
    canSend <- dHold True -< asum [True <$ eclear, False <$ ea']
  returnA -< ea'


doCollision :: SF (Name, V3 Double) (V3 Double)
doCollision = proc (me, here) -> do
  g <- global -< ()
  dpos <- integral -< normalize $ set _z 0 $ sum $ do
    (who@Player{}, o) <- M.toList $ g_everyone g
    guard $ who /= me
    Just cap <- pure $ os_collision o
    Just pos <- pure $ os_pos o
    guard $ capsuleInCapsule (playerCapsule here) cap
    let dist = set _z 0 $ here - pos
    pure $ normalize dist
  returnA -< dpos


wrapPlayer
    :: SF (ObjInput, PlayerState) Controller
    -> SF (Controller, ObjInput, PlayerState) (ObjOutput, PlayerState)
    -> Obj PlayerState
wrapPlayer getCtrls sf = proc (oi, ps) -> do
  ctrl <- getCtrls -< (oi, ps)
  (oo, ps') <- sf -< (ctrl, oi, ps)


  let pass = PassTo (V3 0 0 0) <$ gate (c_pass ctrl) (ps_hasBall ps)
      shoot = ShootAt (V3 (-12) 0 4) <$ gate (c_shoot ctrl) (ps_hasBall ps)

  couldPickup <- onMail @PickMeUp -< oi
  afterwards <- delayEvent 0.5 -< pass <|> shoot
  pickup <- onceUntil -< (couldPickup, afterwards)
  g <- global -< ()

  integral -< normalize $ set _z 0 $ sum $ do
    (who@Player{}, o) <- M.toList $ g_everyone g
    guard $ who /= oi_me oi
    Just cap <- pure $ os_collision o
    Just pos <- pure $ os_pos o
    guard $ capsuleInCapsule (playerCapsule $ ps_pos ps) cap
    let dist = set _z 0 $ ps_pos ps - pos
    pure $ normalize dist

  returnA -<
    ( ( oo <> mempty
        { oo_outbox = mconcat
            [ on pickup $ respond PickedUp
            , on pickup $ const $ send Camera RefocusOnMe
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
      , ps'
        & #ps_hasBall %~ appEndo (
              mconcat
                [ on pickup (const $ Endo $ const True)
                , on pass   (const $ Endo $ const False)
                , on shoot  (const $ Endo $ const False)
                ])
      )
    )


playerController :: SF (ObjInput, PlayerState) Controller
playerController = proc (oi, _) -> inputToController -< oi

stupidController :: SF (ObjInput, PlayerState) Controller
stupidController = proc _ -> do
  returnA -< Controller
    { c_dir = 0
    , c_jump = NoEvent
    , c_shoot = NoEvent
    , c_pass = NoEvent
    , c_run = False
    }

behindController :: V3 Double -> SF (ObjInput, PlayerState) Controller
behindController offset = proc (_, ps) -> do
  g <- global -< ()
  CamPos cam <- getCamera -< ()
  let ballPos = getBall g
  recvBall <- edge -< ps_hasBall ps
  doJump <- delay 1 NoEvent -< recvBall
  doShoot <- delay 0.5 NoEvent -< doJump
  running <- fmap ((0 >=) . sin . (* 2)) time -< ()

  returnA -< Controller
    { c_dir = normalize $ view _xy $
      (case ballPos of
          Just x -> x
          Nothing -> bool (cam + offset) 0 (ps_hasBall ps)
      ) - ps_pos ps
    , c_jump = doJump
    , c_shoot = doShoot
    , c_pass = NoEvent
    , c_run = running
    }

keep :: SF a a
keep = hold (error "keep") <<< snap

runPlayer :: SF (Controller, ObjInput, PlayerState) ((ObjOutput, PlayerState), Event PlayerAction)
runPlayer = proc (ctrl, oi, ps) -> do
  rendered <- renderPlayer -< (OnGround, oi, ps)
  pos0 <- keep -< ps_pos ps
  dpos <- integral -< (0 & _xy .~ c_dir ctrl) ^* bool walkSpeed runSpeed (c_run ctrl)
  cpos <- doCollision -< (oi_me oi, ps_pos ps)

  returnA -< (, asum [ Jump <$ c_jump ctrl ]) $
    ( mempty
        { oo_output = rendered
        }
    , ps
        & #ps_pos .~ pos0 + dpos + cpos
    )


data GroundState = OnGround | Jumping
  deriving stock (Eq, Ord, Show)

ballPosX :: Double
ballPosX = 0.25

filterZero :: Event Double -> Event Double
filterZero (Event 0) = NoEvent
filterZero x = x


standAnim :: Anim
standAnim = Anim "stand" 1

dstandAnim :: Anim
dstandAnim = Anim "dst" 0.09

runAnim :: Anim
runAnim = Anim "run" 0.09

drunAnim :: Anim
drunAnim = Anim "drun" 0.09


renderPlayer :: SF (GroundState, ObjInput, PlayerState) Output
renderPlayer = proc (gs, oi, ps) -> do
  cam <- getCamera -< ()
  old <- iPre 0 -< ps_pos ps ^. _x
  balldir <- hold 1 <<< arr filterZero <<< onChange -< signum $ ps_pos ps ^. _x - old
  let spos@(V2 scx scy) = toScreenNormalized cam $ ps_pos ps
      on_screen = and
        [ -deadzone <= scx
        , scx <= deadzone
        , -deadzone <= scy
        , scy <= deadzone
        ]
      color = teamColor $ oi_me oi

  case on_screen of
    True -> do
      ballZ <- fmap (abs . cos . (* 8)) time -< ()
      let depth = DDDepth $ view _y $ ps_pos ps
      anim
        <- animate gfx_player
        -< bool dstandAnim drunAnim $ ps_hasBall ps
      returnA -< mconcat
        [ drawCapsule cam
            (playerCapsule $ ps_pos ps)
            color
            depth
        , flip (bool mempty) (ps_hasBall ps) $
            drawCapsule cam
              (ballCapsule $ (ps_pos ps + V3 (balldir * ballPosX) 0 0) & _z +~ bool shootHeight ballZ (gs == OnGround))
              (V4 255 128 0 255)
              depth
        , drawAnimation
            anim
            cam
            (ps_pos ps)
            (V2 (balldir < 0) False)
            depth
        ]
    False -> do
      let screenpos
            = (V2 (windowWidth / 2) (windowHeight / 2) *)
            $ fmap (clamp (-in_screen_zone, in_screen_zone)) spos + 1
          dir = (atan2 scy scx * 180 / pi) + 180
      returnA -< mconcat
        [ flip raw DDGUI $ \renderer _ -> do
            fillPie
              renderer
              (fmap round $ screenpos + normalize spos * pie_offset)
              pie_radius
              (round $ dir - pie_arc)
              (round $ dir + pie_arc)
              color
            fillCircle
              renderer
              (fmap round $ screenpos - normalize spos * circle_offset)
              circle_radius
              color
        ]
  where
    deadzone = 1.04
    in_screen_zone = 0.925
    pie_radius = 30
    pie_arc = 45 / 2
    pie_offset = 20
    circle_offset = 8
    circle_radius = 11


nearestTeammate :: Global -> ObjInput -> ObjState
nearestTeammate g oi = fromMaybe (error "no teammate?") $ do
  me@(Player meteam _) <- pure $ oi_me oi
  mepos <- os_pos =<< M.lookup me (g_everyone g)
  fmap snd $ listToMaybe $ sortOn fst $ do
    (name@(Player team _), os) <- M.toList $ g_everyone g
    guard $ team == meteam && name /= me
    pos <- maybeToList $  os_pos os
    pure (qd mepos pos, os)


mkShootBezier :: Global -> ObjInput -> Team -> Bezier Double (V3 Double)
mkShootBezier g oi t = bezier
  [ (fromMaybe (error $ "no pos for me " <> show (oi_me oi)) $
      os_pos =<< M.lookup (oi_me oi) (g_everyone g)) + V3 0 0 shootHeight
  , netPos g t + V3 0 0 3
  , netPos g t
  ]

