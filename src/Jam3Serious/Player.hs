module Jam3Serious.Player where

import Data.Map.Monoidal qualified as MM
import Jam3Serious.Ball
import Jam3Serious.Prelude
import qualified SDL


onPress :: Num a => SDL.Scancode -> a -> Input -> a
onPress field a = flip i_keyboard field >>> arr (bool 0 a)


arrows :: Num a => Input -> V2 a
arrows =
  sum
    [ onPress SDL.ScancodeUp    (V2 0    (-1))
    , onPress SDL.ScancodeDown  (V2 0    1)
    , onPress SDL.ScancodeLeft  (V2 (-1) 0)
    , onPress SDL.ScancodeRight (V2 1    0)
    ]

data Controller = Controller
  { c_dpos :: V2 Double
  , c_shoot :: Bool
  , c_run :: Bool
  }

data PlayerState = PlayerState
  { ps_location :: V2 Double
  -- , ps_collision :: OriginRect Double
  , ps_color :: V4 Word8
  }
  deriving Generic

instance ToObjState PlayerState where
  toObjState ps = ObjState
    { os_pos = Just $ ps_location ps
    , os_collision = Nothing -- Just $ ps_collision ps
    }


player :: Obj PlayerState
player = proc (oi, ps) -> do
  dpos <- arr arrows -< oi_input oi
  let V2 x y = ps_location ps

  let e = mapMaybe (fromDynamic @PickMeUp . message) $ oi_inbox oi

  returnA -<
    ( mempty
        { oo_output = raw $ \r -> do
            SDL.rendererDrawColor r SDL.$= (ps_color ps)
            SDL.fillRect r $ Just $
              SDL.Rectangle
                (SDL.P (fmap round $ V2 x y))
                (V2 50 50)
        , oo_outbox =
            case null e of
              True -> mempty
              False -> MM.singleton Ball $ pure $ toDyn PickedUp
        }
    , ps
        & #ps_location +~ dpos ^* (50 * i_dt (oi_input oi))
        & #ps_color %~
            case null e of
              True -> id
              False -> const $ V4 0 128 255 255
    )

