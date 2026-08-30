module Lib (main) where

import Control.Exception (bracket, bracket_)
import Data.Atlas
import Data.Map qualified as M
import Data.Text (pack)
import FRP.Yampa qualified as Y
import Jam3Serious.Objects.Ball
import Jam3Serious.Objects.Basket
import Jam3Serious.Objects.Camera
import Jam3Serious.Objects.Court
import Jam3Serious.Objects.Player
import Jam3Serious.Prelude
import Jam3Serious.Router
import Jam3Serious.Yampa
import qualified SDL


windowTitle :: String
windowTitle = "3jam3serious"


main :: IO ()
main = bracket_ SDL.initializeAll SDL.quit $ do
  window <- SDL.createWindow
    (pack windowTitle)
    SDL.defaultWindow
      { SDL.windowInitialSize = SDL.V2
          windowWidth
          windowHeight
      }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  gfx <- Gfx <$> loadAtlas renderer "res/player.json"
  bracket (pure (window, renderer))
          (\(w, r) -> SDL.destroyRenderer r >> SDL.destroyWindow w)
          (\_      -> runSF renderer gfx appSF)


appSF :: Y.SF Input Output
appSF = proc i -> do
  let bg = flip raw DDCourt $ \renderer _ -> do
        SDL.rendererDrawColor renderer SDL.$= V4 100 149 237 255
        SDL.clear renderer
  objs
    <- router
        ( flip ObjectMap mempty
        $ M.fromList
            [ (Player T1 $ PlayerNum 0, object (PlayerState (V3 (-1) 0 0) False) $ player playerController)
            , (Court, object () court)
            , (Player T2 $ PlayerNum 1, object (PlayerState (V3 1 0 0) False) $ player stupidController)
            , (Player T2 $ PlayerNum 2, object (PlayerState (V3 1.1 0.1 0) False) $ player stupidController)
            , (Player T2 $ PlayerNum 3, object (PlayerState (V3 1.1 (-0.1) 0) False) $ player stupidController)
            , (Player T2 $ PlayerNum 2, object (PlayerState (V3 1.2 0.2 0) False) $ player stupidController)
            , (Player T2 $ PlayerNum 4, object (PlayerState (V3 1.2 0 0) False) $ player stupidController)
            , (Player T2 $ PlayerNum 5, object (PlayerState (V3 1.2 (-0.2) 0) False) $ player stupidController)
            , (Ball, object (ballState (V3 0 1 2) 0) ball)
            , (Basket T1, object (V3 (-12) 0 4) $ basket $ V3 1 0 0 )
            , (Basket T2, object (V3 12 0 4) $ basket $ V3 (-1) 0 0)
            , (Camera, object (CameraState 0 (50 * 50) 20 Ball) camera)
            ]
        ) -< i

  returnA -< bg <> objs
