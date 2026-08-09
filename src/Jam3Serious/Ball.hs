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
  , bs_collision :: OriginRect Double
  , bs_state :: BState
  }
  deriving Generic

instance ToObjState BallState where
  toObjState ps = ObjState
    { os_pos = Nothing -- Just $ bs_pos ps
    , os_collision = Nothing -- Just $ bs_collision ps
    }

data PickMeUp = PickMeUp
  deriving Show

data PickedUp = PickedUp
  deriving Show

ball :: Obj BallState
ball = proc (oi, bs) -> do
  pickup <- onMail @PickedUp -< oi

  returnA -<
    ( mempty
        { oo_output = raw $ \r -> do
            drawCapsule
              (Capsule 0 0 0.24 $ bs_pos bs)
              (V4 255 128 0 255)
              r
            -- SDL.rendererDrawColor r SDL.$=
            -- SDL.fillRect r $ Just $ fmap round $ mkRect (bs_pos bs) (bs_collision bs)
        -- , oo_outbox =
        --     broadcastAt
        --       (
        --         case bs_state bs of
        --           FreeBall -> has #_Player
        --           Passing from -> \n -> has #_Player n && n /= from
        --       )
        --       PickMeUp
        --       (mkRect (bs_pos bs) (bs_collision bs))
        --       (oi_everyone oi)
        , oo_commands = on pickup $ const $ pure Die
        }
    , bs & #bs_pos +~ bs_vel bs ^* i_dt (oi_input oi)
    )


broadcastAt
    :: Typeable a
    => (Name -> Bool)
    -> a
    -> SDL.Rectangle Double
    -> Map Name ObjState
    -> MonoidalMap Name [Dynamic]
broadcastAt p a rect oss = MM.fromList $ do
  (who, os) <- M.toList oss
  guard $ p who
  pos <- maybeToList $ os_pos os
  guard $ posInRect pos rect

  pure (who, pure $ toDyn a)


ballState :: V3 Double -> V3 Double -> BState -> BallState
ballState pos dir bs = BallState
  { bs_pos = pos
  , bs_vel = dir
  , bs_collision = OriginRect 0 10
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

