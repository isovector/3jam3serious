module Jam3Serious.Ball where

import Jam3Serious.Mail
import Data.Map qualified as M
import Data.Map.Monoidal qualified as MM
import Jam3Serious.Prelude
import qualified SDL

data BallState = BallState
  { bs_pos :: V2 Double
  , bs_collision :: OriginRect Double
  }
  deriving Generic

instance ToObjState BallState where
  toObjState ps = ObjState
    { os_pos = Just $ bs_pos ps
    , os_collision = Just $ bs_collision ps
    }

data PickMeUp = PickMeUp
  deriving Show

data PickedUp = PickedUp
  deriving Show

ball :: Obj BallState
ball = proc (oi, bs) -> do

  pickup <- onMail @PickedUp -< oi

  t <- time -< ()

  returnA -<
    ( mempty
        { oo_output = raw $ \r -> do
            SDL.rendererDrawColor r SDL.$= (V4 255 128 0 255)
            SDL.fillRect r $ Just $ fmap round $ mkRect (bs_pos bs) (bs_collision bs)
        , oo_outbox =
            broadcastAt
              #_Player
              PickMeUp
              (mkRect (bs_pos bs) (bs_collision bs))
              (oi_everyone oi)
        , oo_commands = on pickup $ const $ pure Die
        }
    , bs & #bs_pos . _x .~ cos t * 200
    )


broadcastAt
    :: Typeable a
    => Prism' Name x
    -> a
    -> SDL.Rectangle Double
    -> Map Name ObjState
    -> MonoidalMap Name [Dynamic]
broadcastAt prism a rect oss = MM.fromList $ do
  (who, os) <- M.toList oss
  guard $ has prism who
  pos <- maybeToList $ os_pos os
  guard $ posInRect pos rect

  pure (who, pure $ toDyn a)


mkRect :: Num a => V2 a -> OriginRect a -> SDL.Rectangle a
mkRect xy (OriginRect origin sz) = SDL.Rectangle (SDL.P $ xy - origin) sz


posInRect :: (Ord a, Num a) => V2 a -> SDL.Rectangle a -> Bool
posInRect (V2 x y) (SDL.Rectangle (SDL.P (V2 l t)) (V2 w h)) = and
  [ l <= x
  , t <= y
  , x <= l + w
  , y <= t + h
  ]

