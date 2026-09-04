module Jam3Serious.Objects.Basket where

import Jam3Serious.Collisions
import Jam3Serious.Drawing
import Jam3Serious.Mail
import Jam3Serious.Objects.Ball (getBall)
import Jam3Serious.Objects.Game
import Jam3Serious.Prelude


basketWidth, basketHeight :: Num a => a
basketWidth = 40
basketHeight = 20


basket :: Team -> V3 Double -> Obj (V3 Double)
basket points_for normal = proc (_, pos) -> do
  let bb = basketRect normal $ pos - normal * 0.5
  cam <- getCamera -< ()
  ball <- getBall -< ()
  through <- edge -< pos ^. _z > ball ^. _z
  let goal = gate through $ qd (ball & _z .~ 0) (pos & _z .~ 0) <= netRadius * netRadius

  returnA -<
    ( mempty
        { oo_output =
            mconcat
              [ billboard cam bb (V4 128 128 0 255) (DDDepth $ view _y pos)
              ]
        , oo_outbox = on (Goal points_for <$ goal) $ send Game
        }
    , pos
    )


netRadius :: Double
netRadius = 0.450


mkRim :: Int -> V3 Double -> [V3 Double]
mkRim n o = do
  let n_ = fromIntegral n
  i <- fmap fromIntegral [0 .. n - 1]
  let x = cos (2 * pi / n_ * i) * netRadius
      y = sin (2 * pi / n_ * i) * netRadius
  pure $ o + V3 x y 0

