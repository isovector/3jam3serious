module Lib (main) where

import Control.Exception (bracket, bracket_)
import Data.Map qualified as M
import Data.Text (pack)
import FRP.Yampa
import Jam3Serious.Objects.Camera
import Jam3Serious.Objects.Ball
import Jam3Serious.Objects.Basket
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
  bracket (pure (window, renderer))
          (\(w, r) -> SDL.destroyRenderer r >> SDL.destroyWindow w)
          (\_      -> runSF renderer appSF)


appSF :: SF Input Output
appSF = proc i -> do
  let bg = raw $ \renderer -> do
        SDL.rendererDrawColor renderer SDL.$= V4 100 149 237 255
        SDL.clear renderer
  objs
    <- router
        ( flip ObjectMap mempty
        $ M.fromList
            -- [ (Player T1 $ PlayerNum 0, object (PlayerState (V3 (-1) 0 0) True False) player)
            -- , (Player T1 $ PlayerNum 1, object (PlayerState (V3 2 (-2) 0) False False) player)
            -- , (Player T1 $ PlayerNum 2, object (PlayerState (V3 (-2) 2 0) False False) player)
            -- , (Player T2 $ PlayerNum 0, object (PlayerState (V3 0 0 0) False False) player)
            -- , (Player T2 $ PlayerNum 1, object (PlayerState (V3 2 2 0) False False) player)
            [ (Ball, object (ballState (V3 0 0 2) 0) ball)
            , (Basket T1, object (V3 (-5) 0 4) $ basket $ V3 1 0 0 )
            , (Basket T2, object (V3 5 0 4) $ basket $ V3 (-1) 0 0)
            , (Camera, object (CameraState 0 (50 * 50) 10 Ball) camera)
            ]
        ) -< i

  returnA -< bg <> objs
