module Jam3Serious.Collisions where

import Jam3Serious.Prelude


ballCapsule :: V3 Double -> Capsule Double
ballCapsule = Capsule 0 0 0.24


playerCapsule :: V3 Double -> Capsule Double
playerCapsule = Capsule 2 0.1 0.25

