module Jam3Serious.Prelude
  ( module X
  , module Jam3Serious.Prelude
  ) where

import Control.Applicative as X
import Control.Arrow as X
import Control.Lens as X ((+~), (<>~), (.~), (%~), (#), (&), view, set, over, (^.), (^..), Lens', Prism', Traversal', has)
import Control.Monad as X
import Data.Bool as X (bool)
import Data.Dynamic as X (Dynamic, toDyn, fromDynamic, Typeable)
import Data.Generics.Labels ()
import Data.Monoid as X
import Data.Map as X (Map)
import Data.Map.Monoidal as X (MonoidalMap)
import Data.Maybe as X
import Data.Set as X (Set)
import Data.Word as X
import FRP.Yampa as X hiding ((^/), (^+^), (^-^), (*^), normalize, dot, norm)
import GHC.Generics as X (Generic, Generically(..))
import Jam3Serious.Types as X
import Linear.Metric as X
import Linear.V2 as X
import Linear.V3 as X
import Linear.V4 as X
import Linear.Vector as X
import SDL as X (MouseButton(..), Point(..), unP)
import SDL.Input.Keyboard.Codes as X


mousePos :: SF Input (V2 Int)
mousePos = arr $ unP . i_mousepos


mouseBtn :: MouseButton -> SF Input Bool
mouseBtn = arr . flip i_mouse


keyboard :: Scancode -> SF Input Bool
keyboard = arr . flip i_keyboard


keypress :: Scancode -> SF Input (Event ())
keypress c = keyboard c >>> edge


object :: ToObjState a => a -> Obj a -> Object
object a0 obj = loopPre a0 $ obj >>> arr (\(oo, a) -> ((oo, toObjState a), a))

