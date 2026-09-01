module Jam3Serious.Objects.Basket where

import Data.Map qualified as M
import Jam3Serious.Collisions
import Jam3Serious.Drawing
import Jam3Serious.Geometry
import Jam3Serious.Objects.Camera
import Jam3Serious.Prelude
import Jam3Serious.Mail
import SDL.Primitive


basketWidth, basketHeight :: Num a => a
basketWidth = 40
basketHeight = 20


basketRect :: V3 Double -> V3 Double -> Rect3 Double
basketRect normal pos = do
  let n = normalize normal
      up = V3 0 0 1
      uDir = normalize $ cross up n
      vDir = normalize $ cross n uDir
      u = uDir ^* 1.83 / 2
      v = vDir ^* 1.07 / 2
  Rect3 pos u v


basket :: V3 Double -> Obj (V3 Double)
basket normal = proc (_, pos) -> do
  let bb = basketRect normal $ pos - normal * 0.5
  cam <- getCamera -< ()
  mball <- friend (\n o -> os_pos =<< bool Nothing (Just o) (n == Ball)) -< ()
  ball_vel <- derivative -< fromMaybe 0 mball

  let maybe_goal =
        maybeToEvent $ do
          ball <- mball
          case pointInCapsule (pos - V3 0 0 0.1) (ballCapsule ball) && dot (V3 0 0 (-1)) ball_vel > 0 of
            True -> pure ()
            False -> Nothing

  goal <- onlyEvery 2 -< maybe_goal


  returnA -<
    ( mempty
        { oo_output =
            mconcat
              [ billboard cam bb (V4 128 128 0 255) (DDDepth $ view _y pos)
              , flip raw (DDDepth $ view _y pos) $ \r _ -> do
                  ellipse r
                    (fmap round $ screenPos $ toScreen cam pos) 40 10 $ V4 255 0 0 255
              ]
        , oo_outbox = on goal $ send Ball
        }
    , pos
    )


netPos :: Global -> Team -> V3 Double
netPos g t =
  fromMaybe (error $ "no net for team " <> show t) $
    os_pos =<< M.lookup (Basket t) (g_everyone g)


netRadius :: Double
netRadius = 0.450


mkRim :: Int -> V3 Double -> [V3 Double]
mkRim n o = do
  let n_ = fromIntegral n
  i <- fmap fromIntegral [0 .. n - 1]
  let x = cos (2 * pi / n_ * i) * netRadius
      y = sin (2 * pi / n_ * i) * netRadius
  pure $ o + V3 x y 0

