module Jam3Serious.Geometry where

import Jam3Serious.Types
import Linear
import FRP.Yampa (edge, Event, gate, returnA)


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


rectCorners :: Num a => Rect3 a -> V4 (V3 a)
rectCorners (Rect3 c u v) =
  V4
    (c - u - v)
    (c - u + v)
    (c + u + v)
    (c + u - v)


rectNormal :: (Floating a, Epsilon a) => Rect3 a -> V3 a
rectNormal (Rect3 _ u v) = normalize $ cross u v


-- | The result of 'rect3V3Check'.
data Prism3 a = Prism3
  { p3_inside :: Bool
  , p3_distance :: a
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)


-- | Determine where a 'V3' lies in relation to a 'Rect3'.
rect3V3Check :: (Floating a, Epsilon a, Ord a) => Rect3 a -> V3 a -> Prism3 a
rect3V3Check r@(Rect3 c u v) p = do
  let -- Normalize @p@ in terms of @c@
      d = p - c
      -- Normalize @d@ in terms of its @u@ and @v@ dimensions.
      a = dot d u / dot u u
      b = dot d v / dot v v
      -- Get the distance of @d@ along the normal
      s = dot d $ rectNormal r
  Prism3
    { p3_inside =
        -- Our rectangle has been normalized such that both a and b range
        -- between @[-1, a]@.
        abs a <= 1 && abs b <= 1
    , p3_distance = s
    }


-- | Reflect a 'V3' off of a 'Rect3'.
rect3Reflect :: (Floating a, Epsilon a) => Rect3 a -> V3 a -> V3 a
rect3Reflect r d =  do
  let n = rectNormal r
  d - 2 * dot d n *^ n


-- | Determine if a changing position ought to bounce off a static 'Rect3'.
-- Returns an event that will reflect the velocity.
rect3Bounce :: (Floating a, Epsilon a, Ord a) => Rect3 a -> SF (V3 a) (Event (V3 a -> V3 a))
rect3Bounce rect = proc pos -> do
  let p3 = rect3V3Check rect pos
  couldBounce <- edge -< p3_distance p3 <= 0
  returnA -< rect3Reflect rect <$ gate couldBounce (p3_inside p3)

