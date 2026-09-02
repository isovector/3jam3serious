module Jam3Serious.Objects.Game where

import Data.Map qualified as M
import Data.Map.Monoidal.Strict qualified as MM
import Jam3Serious.Collisions
import Jam3Serious.Drawing
import Jam3Serious.Geometry
import Jam3Serious.Objects.Camera
import Jam3Serious.Prelude
import Jam3Serious.Mail
import SDL.Primitive

data Goal = Goal Team


game :: Obj ()
game = proc (oi, st) -> do
  goal <- onMail @Goal -< oi

  score <- accumHoldBy
    (\s (Goal t) -> s <> MM.singleton t (Sum 1))
    (MM.singleton T1 0 <> MM.singleton T2 0) -< fmap message goal

  returnA -<
    ( mempty
        { oo_output = mconcat
            [ drawText gfx_numbers (V2 20 20) $ getSum $ score MM.! T1
            , drawText gfx_numbers (V2 20 50) $ getSum $ score MM.! T2
            ]
        }
    , st
    )
