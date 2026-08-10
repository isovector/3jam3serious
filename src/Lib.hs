module Lib (main) where

import Control.Exception (bracket, bracket_)
import Data.Map qualified as M
import Data.Text (pack)
import FRP.Yampa
import Jam3Serious.Ball
import Jam3Serious.Camera
import Jam3Serious.Player
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
            [ (Player T1 P1, object (PlayerState (V3 (-1) 0 0) True) player)
            , (Player T1 P2, object (PlayerState (V3 1 0 0) False) player)
            , (Ball, object (ballState (V3 0 0 1) 0 FreeBall) ball)
            ]
        ) -< i

  returnA -< bg <> objs
