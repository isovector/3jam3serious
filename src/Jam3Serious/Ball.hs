module Jam3Serious.Ball where

import Control.Monad
import Data.Dynamic
import Data.Map qualified as M
import Data.Map.Monoidal qualified as MM
import Data.Maybe
import Jam3Serious.Prelude
import qualified SDL

data BallState = BallState
  { bs_pos :: V2 Double
  , bs_collision :: OriginRect Double
  }

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

  let e = mapMaybe (fromDynamic @PickedUp . message) $ oi_inbox oi
  color <- hold (V4 255 128 0 255) -< maybe mempty pure $ (0 <$) $ listToMaybe e

  returnA -<
    ( mempty
        { oo_output = raw $ \r -> do
            SDL.rendererDrawColor r SDL.$= color
            SDL.fillRect r $ Just $ fmap round $ mkRect (bs_pos bs) (bs_collision bs)
        , oo_outbox =
            broadcastAt
              PickMeUp
              (mkRect (bs_pos bs) (bs_collision bs))
              (oi_everyone oi)
        }
    , bs
    )


broadcastAt :: Typeable a => a -> SDL.Rectangle Double -> Map Name ObjState -> MonoidalMap Name [Dynamic]
broadcastAt a rect oss = MM.fromList $ do
  (who, os) <- M.toList oss
  guard $ who /= Ball
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

