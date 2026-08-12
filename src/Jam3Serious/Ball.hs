module Jam3Serious.Ball where

import Data.Bezier
import Data.Monoid
import Data.Map qualified as M
import Data.Map.Monoidal qualified as MM
import Jam3Serious.Drawing
import Jam3Serious.Geometry
import Jam3Serious.Mail
import Jam3Serious.Prelude
import qualified SDL
import FRP.Yampa qualified as Y


data BState
  = FreeBall
  | Passing Name
  deriving stock (Generic)

data FollowBezier = FollowBezier
  { fb_dur :: Double
  , fb_bez :: Bezier Double (V3 Double)
  }
  deriving (Show)

data BallState = BallState
  { bs_pos :: V3 Double
  , bs_vel :: V3 Double
  , bs_state :: BState
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

instance VectorSpace (V3 Double) Double where
  zeroVector = 0
  (*^) = (*^)
  (^+^) = (+)
  dot = dot

ballBezier :: FollowBezier -> ObjE BallState ()
ballBezier (FollowBezier dur bez) = proc (_, bs) -> do
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

ball :: Obj BallState
ball = foreverSwont $ do
  e <- swont ballReg
  swont $ ballBezier e

drawBall :: BallState -> Output
drawBall bs =
  raw $ \r -> do
    drawCapsule
      (ballCapsule $ bs_pos bs)
      (V4 255 128 0 255)
      r

ballReg :: ObjE BallState FollowBezier
ballReg = proc (oi, bs) -> do
  pickup <- onMail @PickedUp -< oi
  follow <- onMail @FollowBezier -< oi

  bounce <- edge -< view _z (bs_pos bs) <= 0

  returnA -<
    (
      ( mempty
          { oo_output = drawBall bs
          , oo_outbox =
              broadcastAt
                (
                  case bs_state bs of
                    FreeBall -> has #_Player
                    Passing from -> \n -> has #_Player n && n /= from
                )
                PickMeUp
                (ballCapsule $ bs_pos bs)
                (oi_everyone oi)
          , oo_commands = on pickup $ const $ pure Die
          }
      , bs
          & #bs_pos +~ bs_vel bs ^* i_dt (oi_input oi)
          & #bs_vel +~ ballGravity ^* i_dt (oi_input oi)
          & #bs_vel %~ appEndo (on bounce $ const $ Endo $ (V3 id id ((ballElasticity *) . negate) <*>))
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


ballState :: V3 Double -> V3 Double -> BState -> BallState
ballState pos dir bs = BallState
  { bs_pos = pos
  , bs_vel = dir
  , bs_state = bs
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

