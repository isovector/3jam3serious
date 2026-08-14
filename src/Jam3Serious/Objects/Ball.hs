module Jam3Serious.Objects.Ball where

import GHC.Generics
import Data.Bezier
import Data.Map qualified as M
import Data.Map.Monoidal qualified as MM
import Data.Monoid
import Jam3Serious.Drawing
import Jam3Serious.Geometry
import Jam3Serious.Mail
import Jam3Serious.Prelude
import qualified SDL

data FollowBezier = FollowBezier
  { fb_dur :: Double
  , fb_bez :: Bezier Double (V3 Double)
  }
  deriving (Show)

data BallState = BallState
  { bs_pos :: V3 Double
  , bs_vel :: V3 Double
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
motionBall dur bez = proc (_, bs) -> do
  t <- time -< ()
  done <- after dur () -< ()
  vel <- derivative -< bs_pos bs
  returnA -<
    ( ( mempty { oo_output = drawBall bs }
      , bs
          & #bs_pos .~ runBezier bez (t / dur)
          & #bs_vel .~ vel
      )
    , done
    )

getBallPos :: ObjE BallState (V3 Double)
getBallPos = arr $ \(_, bs) -> ((mempty, bs), pure $ bs_pos bs)

ball :: Obj BallState
ball = foreverSwont $ do
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

drawBall :: BallState -> Output
drawBall bs =
  drawCapsule
    (ballCapsule $ bs_pos bs)
    (V4 255 128 0 255)


data BallAction
  = ShootAt (V3 Double)
  | PassTo (V3 Double)
  deriving stock (Eq, Ord, Show)

courtFloor :: Rect3 Double
courtFloor = Rect3 0 (V3 (28.65 / 2) 0 0) (V3 0 (15.24 / 2) 0)

physicsBall :: ObjE BallState BallAction
physicsBall = proc (oi, bs) -> do
  pickup <- onMail @PickedUp -< oi
  follow <- onMail @BallAction -< oi
  bounce <- rect3Bounce courtFloor -< bs_pos bs


  returnA -<
    (
      ( mempty
          { oo_output = mconcat
              [ billboard courtFloor $ V4 92 92 92 255
              , drawBall bs
              ]
          , oo_outbox = mempty
              -- broadcastAt
              --   (
              --     case bs_state bs of
              --       FreeBall -> has #_Player
              --       Passing from -> \n -> has #_Player n && n /= from
              --   )
              --   PickMeUp
              --   (ballCapsule $ bs_pos bs)
              --   (oi_everyone oi)
          , oo_commands = on pickup $ const $ pure Die
          }
      , bs
          & #bs_vel +~ ballGravity ^* i_dt (oi_input oi)
          & #bs_vel %~ appEndo (on bounce $ \f -> Endo $ f . (^* ballElasticity))
          & #bs_pos +~ bs_vel bs ^* i_dt (oi_input oi)
      )
    , fmap message follow
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


mkRect :: Num a => V2 a -> OriginRect a -> SDL.Rectangle a
mkRect xy (OriginRect origin sz) = SDL.Rectangle (SDL.P $ xy - origin) sz


posInRect :: (Ord a, Num a) => V2 a -> SDL.Rectangle a -> Bool
posInRect (V2 x y) (SDL.Rectangle (SDL.P (V2 l t)) (V2 w h)) = and
  [ l <= x
  , t <= y
  , x <= l + w
  , y <= t + h
  ]

