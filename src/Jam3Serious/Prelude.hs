module Jam3Serious.Prelude
  ( module X
  , module Jam3Serious.Prelude
  ) where

import Control.Applicative as X
import Control.Arrow as X
import Control.Lens as X ((+~), (<>~), (.~), (%~), (#), (&), view, set, over, (^.), (^..), Lens', Prism', Traversal', has, _1, _2, _3)
import Control.Monad as X
import Control.Monad.Cont
import Data.Bool as X (bool)
import Data.Dynamic as X (Dynamic, toDyn, fromDynamic, Typeable)
import Data.Generics.Labels ()
import Data.Map as X (Map)
import Data.Map.Monoidal.Strict as X (MonoidalMap)
import Data.Map.Strict qualified as M
import Data.Maybe as X
import Data.Monoid as X
import Data.Set as X (Set)
import Data.Word as X
import Debug.Trace as X (trace, traceShowId)
import FRP.SFGlobal as X
import FRP.Yampa as X (Event(..), noEvent, maybeToEvent)
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
mousePos = arr $ unP . m_mousepos . i_mouse


mouseBtn :: MouseButton -> SF Input Bool
mouseBtn = arr . flip (m_mouse . i_mouse)


keyboard :: Scancode -> SF Input Bool
keyboard = arr . flip (checkKeyboard . i_keyboard)


keypress :: Scancode -> SF Input (Event ())
keypress c = keyboard c >>> edge


object :: ToObjState a => a -> Obj a -> Object
object a0 obj = loopPre a0 $ obj >>> arr (\(oo, a) -> ((oo, toObjState a), a))


onChange :: Eq a => SF a (Event a)
onChange = proc a ->
  edgeBy (\old new -> bool Nothing new $ old /= new) Nothing -< Just a


timeout :: Time -> ObjSwont a b -> ObjSwont a (Maybe b)
timeout t m = Swont $ cont $ \k ->
  dSwitch
    ( proc i -> do
        done <- after t Nothing -< ()
        o <- switchSwont (k . Just) m -< i
        returnA -< (o, done)
    ) k


traceEventF :: Show b => (a -> b) -> Event a -> Event a
traceEventF f (Event a) = trace (show $ f a) $ Event a
traceEventF _ NoEvent = NoEvent

traceEvent :: Show a => Event a -> Event a
traceEvent (Event a) = traceShowId $ Event a
traceEvent NoEvent = NoEvent


friends :: (a -> Name -> ObjState -> Maybe b) -> SF a [b]
friends f = proc a -> do
  g <- global -< ()
  returnA -< mapMaybe (uncurry $ f a) . M.toList $ g_everyone g

friend :: (a -> Name -> ObjState -> Maybe b) -> SF a (Maybe b)
friend f = friends f >>> arr listToMaybe


onceUntil :: SF (Event a, Event clear) (Event a)
onceUntil = proc (ea, eclear) -> do
  rec
    let ea' = gate ea canSend
    canSend <- dHold True -< asum [True <$ eclear, False <$ ea']
  returnA -< ea'

onlyEvery :: DTime -> SF (Event a) (Event a)
onlyEvery dt = proc ev -> do
  clear <- delay dt NoEvent -< ev
  onceUntil -< (ev, clear)

fallingEdge :: SF Bool (Event ())
fallingEdge = edgeBy (\x y -> bool Nothing (Just ()) $ x && not y) True

keep :: SF a a
keep = hold (error "keep") <<< snap

