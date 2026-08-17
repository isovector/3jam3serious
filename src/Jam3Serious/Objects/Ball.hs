module Jam3Serious.Objects.Ball where

import Jam3Serious.Objects.Basket
import GHC.Generics
import Data.Bezier
import Data.Map qualified as M
import Data.Map.Monoidal.Strict qualified as MM
import Data.Monoid
import Jam3Serious.Drawing
import Jam3Serious.Geometry
import Jam3Serious.Mail
import Jam3Serious.Prelude
import qualified SDL

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


ballCapsule :: V3 Double -> Capsule Double
ballCapsule = Capsule 0 0 0.24


ballGravity :: V3 Double
ballGravity = V3 0 0 (-10)

ballElasticity :: Double
ballElasticity = 0.8

motionBall :: Time -> Bezier Double (V3 Double) -> ObjE BallState ()
motionBall dur bez = fmap (fmap void) $ bouncing $ proc (oi, bs) -> do
  t <- time -< ()
  done <- after dur () -< ()
  vel <- derivative -< bs_pos bs

  returnA -<
    ( ( mempty { oo_output = drawBall oi bs }
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
  swont getBallPos >>= flip shootAt (V3 (-5) 0 4)
  timeout 3 $ swont physicsBall
  swont getBallPos >>= flip passTo (V3 (5) 0 4)
  timeout 3 $ swont physicsBall
  swont getBallPos >>= flip passTo (V3 (-5) 0 4)
  timeout 3 $ swont physicsBall
  swont getBallPos >>= flip shootAt (V3 (5) 0 4)
  timeout 3 $ swont physicsBall


  -- e <- swont physicsBall
  -- pos <- swont getBallPos
  -- case e of
  --   PassTo goal -> passTo pos goal
  --   ShootAt goal -> shootAt pos goal


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

drawBall :: ObjInput -> BallState -> Output
drawBall oi bs = mconcat
  [ foldMap (uncurry $ billboard oi) court
  , drawCapsule oi
      (ballCapsule $ bs_pos bs)
      (V4 255 128 0 255)
  ]


data BallAction
  = ShootAt (V3 Double)
  | PassTo (V3 Double)
  deriving stock (Eq, Ord, Show)

court :: [(Rect3 Double, V4 Word8)]
court =
  [ -- floor
    (Rect3 0 (V3 w 0 0) (V3 0 d 0), V4 92 92 92 255)
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

bouncing :: ObjE BallState e -> ObjE BallState (Maybe e)
bouncing sf = proc i -> do
  bounce <- foldMap (\r -> fmap (fmap Endo) $ rect3Bounce r) $ fmap fst court -< bs_pos $ snd i
  oo <- sf -< i
  returnA -<
    oo
      & _1 . _2 . #bs_vel %~ appEndo (on bounce $ \f -> f <> Endo (^* ballElasticity))
      & _2 %~ event (Nothing <$ bounce) (pure . Just)


physicsBall :: ObjE BallState BallAction
physicsBall = fmap (fmap $ (maybe noEvent pure =<<)) $ bouncing $ proc (oi, bs) -> do
  follow <- onMail @BallAction -< oi

  returnA -<
    (
      ( mempty
          { oo_output = drawBall oi bs
          , oo_outbox = mempty
          }
      , bs
          & #bs_vel +~ ballGravity ^* i_dt (oi_input oi)
          & #bs_pos +~ bs_vel bs ^* i_dt (oi_input oi)
      )
    , fmap message follow
    )

doPickup :: Obj BallState -> Obj BallState
doPickup sf = proc i@(oi, bs) -> do
  pickup <- onMail @PickedUp -< oi
  (oo, bs') <- sf -< i
  returnA -<
    ( oo
        { oo_commands = on pickup $ const $ pure Die
        , oo_outbox =
            broadcastAt
              (const True)
              PickMeUp
              (ballCapsule $ bs_pos bs)
              (oi_everyone oi)
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

