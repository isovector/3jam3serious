module FRP.SFGlobal
  ( module FRP.SFGlobal
  , Time
  , Y.DTime
  , Y.event
  , Y.gate
  ) where

import Data.Monoid
import Control.Arrow
import Control.Arrow.Transformer.Reader
import Control.Category
import FRP.Yampa qualified as Y
import Prelude hiding ((.))
import FRP.Yampa (Event(..), Time)
import Data.Coerce


newtype SFG g a b = SFG
  { runGlobal :: Y.SF (a, g) b
  }
  deriving newtype (Functor, Applicative)
  deriving (Category, Arrow, ArrowChoice, ArrowLoop) via ReaderArrow g Y.SF
  deriving (Semigroup, Monoid, Num) via Ap (SFG g a) b


lift :: Y.SF a b -> SFG g a b
lift sf = SFG $ arr fst >>> sf


global :: SFG g a g
global = SFG $ arr snd

switch :: SFG g a (b, Event c) -> (c -> SFG g a b) -> SFG g a b
switch = coerce Y.switch

dSwitch :: SFG g a (b, Event c) -> (c -> SFG g a b) -> SFG g a b
dSwitch = coerce Y.dSwitch

edge :: SFG g Bool (Event ())
edge = SFG $ arr fst >>> Y.edge

edgeBy :: (a -> a -> Maybe b) -> a -> SFG g a (Event b)
edgeBy f a = SFG $ arr fst >>> Y.edgeBy f a

after :: Time -> b -> SFG g a (Event b)
after = coerce Y.after

iPre :: a -> SFG g a a
iPre a = SFG $ arr fst >>> Y.iPre a

now :: b -> SFG g a (Event b)
now = coerce Y.now

notYet :: SFG g (Event a) (Event a)
notYet = SFG $ arr fst >>> Y.notYet

derivative :: (Fractional s, Y.VectorSpace a s) => SFG g a a
derivative = SFG $ arr fst >>> Y.derivative

integral :: (Fractional s, Y.VectorSpace a s) => SFG g a a
integral = SFG $ arr fst >>> Y.integral

time :: SFG g a Time
time = coerce Y.time

delay :: Time -> a -> SFG g a a
delay t a = SFG $ arr fst >>> Y.delay t a

delayEvent :: Time -> SFG g (Event a) (Event a)
delayEvent t = SFG $ arr fst >>> Y.delayEvent t

snap :: SFG g a (Event a)
snap = SFG $ arr fst >>> Y.snap

hold :: a -> SFG g (Event a) a
hold a = SFG $ arr fst >>> Y.hold a

dHold :: a -> SFG g (Event a) a
dHold a = SFG $ arr fst >>> Y.dHold a

loopPre :: c -> SFG g (a, c) (b, c) -> SFG g a b
loopPre c sfg = SFG $ Y.loopPre c (arr reassoc >>> runGlobal sfg)

reassoc :: ((a, b), c) -> ((a, c), b)
reassoc ((a, b), c) = ((a, c), b)

noiseR :: (Y.RandomGen gen, Y.Random b) => (b, b) -> gen -> SFG g a b
noiseR bs g = SFG $ Y.noiseR bs g

accumHoldBy :: (b -> a -> b) -> b -> SFG g (Event a) b
accumHoldBy f b = SFG $ arr fst >>> Y.accumHoldBy f b
