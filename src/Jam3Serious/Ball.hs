module Jam3Serious.Ball where

import Jam3Serious.Geometry
import Jam3Serious.Mail
import Data.Map qualified as M
import Data.Map.Monoidal qualified as MM
import Jam3Serious.Prelude
import qualified SDL

data BState
  = FreeBall
  | Passing Name
  deriving stock (Generic, Eq, Ord, Show)


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

ball :: Obj BallState
ball = proc (oi, bs) -> do
  pickup <- onMail @PickedUp -< oi

  returnA -<
    ( mempty
        { oo_output = raw $ \r -> do
            drawCapsule
              (ballCapsule $ bs_pos bs)
              (V4 255 128 0 255)
              r
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
    , bs & #bs_pos +~ bs_vel bs ^* i_dt (oi_input oi)
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

