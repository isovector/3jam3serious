module Jam3Serious.Collisions where

import Jam3Serious.Prelude


ballCapsule :: V3 Double -> Capsule Double
ballCapsule = Capsule 0 0 0.119


playerCapsule :: V3 Double -> Capsule Double
playerCapsule = Capsule 2 0.1 0.25


basketRect :: V3 Double -> V3 Double -> Rect3 Double
basketRect normal pos = do
  let n = normalize normal
      up = V3 0 0 1
      uDir = normalize $ cross up n
      vDir = normalize $ cross n uDir
      u = uDir ^* 1.83 / 2
      v = vDir ^* 1.07 / 2
  Rect3 pos u v
