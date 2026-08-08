module Jam3Serious.Player where

import Data.Bool
import Control.Arrow
import Control.Lens ((+~), (&))
import Data.Generics.Labels ()
import Data.Word
import FRP.Yampa
import GHC.Generics
import Jam3Serious.Types
import Linear.V2
import Linear.V4
import Linear.Vector ((^*))
import qualified SDL

onPress :: Num a => SDL.Scancode -> a -> SF Input a
onPress field a = keyboard field >>> arr (bool 0 a)

arrows :: Num a => SF Input (V2 a)
arrows =
  sum
    [ onPress SDL.ScancodeUp    (V2 0 (-1))
    , onPress SDL.ScancodeDown  (V2 0 1)
    , onPress SDL.ScancodeLeft  (V2 (-1) 0)
    , onPress SDL.ScancodeRight (V2 1    0)
    ]

data PlayerState = PlayerState
  { ps_location :: V2 Double
  , ps_color :: V4 Word8
  }
  deriving Generic

instance ToObjState PlayerState where
  toObjState _ = ObjState

player :: Obj PlayerState
player = proc (oi, ps) -> do
  dpos <- arrows -< oi_input oi
  let V2 x y = ps_location ps
  returnA -<
    ( mempty
        { oo_output = raw $ \r -> do
            SDL.rendererDrawColor r SDL.$= (ps_color ps)
            SDL.fillRect r $ Just $
              SDL.Rectangle
                (SDL.P (fmap round $ V2 x y))
                (V2 50 50)
        }
    , ps & #ps_location +~ dpos ^* (50 * i_dt (oi_input oi))
    )

