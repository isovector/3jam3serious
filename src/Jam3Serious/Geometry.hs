module Jam3Serious.Geometry where

import Jam3Serious.Types


pointInCapsule :: (Num a, Ord a) => V3 a -> Capsule a -> Bool
pointInCapsule (V3 x y z) (Capsule t b r (V3 cx cy cz)) = do
  let top = cz + t
      bot = cz - b
      dx = (cx - x)
      dy = (cy - y)
      dbz = (cz - b - z)
      dtz = (cz + t - z)
  and
    [ -- Above bottom and below top of the capsule?
      bot - r <= z
    , z <= top + r
    , -- Within the cylinder?
      dx * dx + dy * dy <= r * r
    , -- In the top or bottom hemisphere?
      z < bot ==>
        dx * dx + dy * dy + dbz * dbz <= r * r
    , top < z ==>
        dx * dx + dy * dy + dtz * dtz <= r * r
    ]


capsuleInCapsule :: (Num a, Ord a) => Capsule a -> Capsule a -> Bool
capsuleInCapsule
    (Capsule t1 b1 r1 (V3 x1 y1 z1))
    (Capsule t2 b2 r2 (V3 x2 y2 z2)) = do
  let top1 = z1 + t1
      bot1 = z1 - b1
      top2 = z2 + t2
      bot2 = z2 - b2
      dx = x1 - x2
      dy = y1 - y2
      gap = max 0 $ max (bot2 - top1) (bot1 - top2)
      rsum = r1 + r2
  dx * dx + dy * dy + gap * gap <= rsum * rsum


(==>) :: Bool -> Bool -> Bool
x ==> y = not x || y
infix 0 ==>

