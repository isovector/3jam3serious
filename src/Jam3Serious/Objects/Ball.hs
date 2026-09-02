module Jam3Serious.Objects.Ball where

import Data.Bezier
import Data.Map qualified as M
import Data.Map.Monoidal.Strict qualified as MM
import Data.Monoid
import GHC.Generics
import Jam3Serious.Objects.Camera
import Jam3Serious.Drawing
import Jam3Serious.Geometry
import Jam3Serious.Mail
import Jam3Serious.Objects.Court
import Jam3Serious.Prelude
import Jam3Serious.Collisions


getBall :: Global -> Maybe (V3 Double)
getBall g =
  os_pos =<< M.lookup Ball (g_everyone g)

data FollowBezier = FollowBezier
  { fb_dur :: !Double
  , fb_bez :: !(Bezier Double (V3 Double))
  }
  deriving (Show)

data BallState = BallState
  { bs_pos :: !(V3 Double)
  , bs_vel :: !(V3 Double)
  }
  deriving Generic

instance ToObjState BallState where
  toObjState ps = ObjState
    { os_pos = Just $ bs_pos ps
    , os_collision = Just $ ballCapsule $ bs_pos ps
    }

data PickMeUp = PickMeUp
  deriving Show

data PickedUp = PickedUp
  deriving Show


ballGravity :: V3 Double
ballGravity = V3 0 0 (-10)

ballElasticity :: Double
ballElasticity = 0.8

motionBall :: Time -> Bezier Double (V3 Double) -> ObjE BallState ()
motionBall dur bez = fmap (fmap void) $ bouncing $ proc (_, bs) -> do
  t <- time -< ()
  done <- after dur () -< ()
  vel <- derivative -< bs_pos bs
  cam <- getCamera -< ()

  returnA -<
    ( ( mempty { oo_output = drawBall cam bs }
      , bs
          & #bs_pos .~ runBezier bez (t / dur)
          & #bs_vel .~ vel
      )
    , done
    )

getBallPos :: ObjE BallState (V3 Double)
getBallPos = arr $ \(_, bs) -> ((mempty, bs), pure $ bs_pos bs)

ball :: Obj BallState
ball = doPickup $ foreverSwont $ do
  e <- swont physicsBall
  pos <- swont getBallPos
  case e of
    PassTo goal -> passTo pos goal
    ShootAt goal -> shootAt pos goal


passTo :: V3 Double -> V3 Double -> ObjSwont BallState ()
passTo pos goal =
  swont $ motionBall 1 $ bezier [pos & _z .~ passHeight, goal]

shootAt :: V3 Double -> V3 Double -> ObjSwont BallState ()
shootAt pos goal = do
  let start = pos + V3 0 0 shootHeight
  swont $ motionBall 1 $ bezier
    [ start
    , start + (goal - start) / 3 + midControlOffset
    , start + (goal - start) * (2 / 3) + shootControlOffset
    , goal
    ]

shootHeight, passHeight :: Double
shootHeight = 2.4
passHeight = 1.7

midControlOffset, shootControlOffset :: V3 Double
midControlOffset = V3 0 0 4
shootControlOffset = V3 0 0 3

drawBall :: CamPos -> BallState -> Output
drawBall cam bs = mconcat
  [ drawCapsule cam
      (ballCapsule $ bs_pos bs)
      (V4 255 128 0 255)
      (DDDepth $ view _y $ bs_pos bs)
  ]


data BallAction
  = ShootAt (V3 Double)
  | PassTo (V3 Double)
  deriving stock (Eq, Ord, Show)

bouncing :: ObjE BallState e -> ObjE BallState (Maybe e)
bouncing sf = proc (oi, bs) -> do
  let pos = bs_pos bs
  rims <- friends (\_ n o -> os_pos =<< bool Nothing (Just o) (has #_Rim n)) -< ()
  let rimBounce = maybeToEvent $ getFirst $
        flip foldMap rims $ \rim ->
          case pointInCapsule rim (ballCapsule pos) && dot (bs_vel bs) (rim - pos) > 0 of
            True -> pure $ Endo $ reflectAlong $ normalize $ rim - pos
            False -> mempty

  wallBounce <- foldMap (\r -> fmap (fmap Endo) $ rect3Bounce r) $ fmap fst courtGeom -< pos
  let bounce = rimBounce <|> wallBounce

  oo <- sf -< (oi, bs)
  returnA -<
    oo
      & _1 . _2 . #bs_vel %~ appEndo (on bounce $ \f -> f <> Endo (^* ballElasticity))
      & _2 %~ event (Nothing <$ bounce) (pure . Just)


physicsBall :: ObjE BallState BallAction
physicsBall = fmap (fmap $ (maybe noEvent pure =<<)) $ bouncing $ proc (oi, bs) -> do
  follow <- onMail @BallAction -< oi
  cam <- getCamera -< ()

  returnA -<
    (
      ( mempty
          { oo_output = drawBall cam bs
          }
      , bs
          & #bs_vel +~ ballGravity ^* i_dt (oi_input oi)
          & #bs_pos +~ bs_vel bs ^* i_dt (oi_input oi)
      )
    , fmap message follow
    )

doPickup :: Obj BallState -> Obj BallState
doPickup sf = proc i@(oi, bs) -> do
  spawn <- now () -< ()
  pickup <- onMail @PickedUp -< oi
  (oo, bs') <- sf -< i
  g <- global -< ()
  returnA -<
    ( oo <> mempty
        { oo_commands = on pickup $ const $ pure Die
        , oo_outbox = mconcat
            [ broadcastAt
                (has #_Player)
                PickMeUp
                (ballCapsule $ bs_pos bs)
                (g_everyone g)
            , on spawn $ const $ send Camera RefocusOnMe
            ]
        }
    , bs'
    )


broadcastAt
    :: Typeable a
    => (Name -> Bool)
    -> a
    -> Capsule Double
    -> Map Name ObjState
    -> MonoidalMap Name [Dynamic]
broadcastAt p a cap oss = MM.fromList $ do
  (who, os) <- M.toList oss
  guard $ p who
  cap' <- maybeToList $ os_collision os
  guard $ capsuleInCapsule cap cap'

  pure (who, pure $ toDyn a)


ballState :: V3 Double -> V3 Double -> BallState
ballState pos dir = BallState
  { bs_pos = pos
  , bs_vel = dir
  }

