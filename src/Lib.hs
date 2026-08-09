module Lib (main) where

import Control.Arrow
import Control.Exception (bracket, bracket_)
import Data.Map qualified as M
import Data.Text (pack)
import FRP.Yampa
import Jam3Serious.Player
import Jam3Serious.Ball
import Jam3Serious.Router
import Jam3Serious.Types
import Jam3Serious.Yampa
import qualified SDL


windowTitle :: String
windowTitle = "3jam3serious"


windowWidth, windowHeight :: Int
windowWidth  = 800
windowHeight = 600


main :: IO ()
main = bracket_ SDL.initializeAll SDL.quit $ do
  window <- SDL.createWindow
    (pack windowTitle)
    SDL.defaultWindow
      { SDL.windowInitialSize = SDL.V2
          (fromIntegral windowWidth)
          (fromIntegral windowHeight)
      }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  bracket (pure (window, renderer))
          (\(w, r) -> SDL.destroyRenderer r >> SDL.destroyWindow w)
          (\_      -> runSF renderer appSF)


appSF :: SF Input Output
appSF = proc i -> do
  let bg = raw $ \renderer -> do
        SDL.rendererDrawColor renderer SDL.$= 0
        SDL.clear renderer
  objs
    <- router
        ( flip ObjectMap mempty
        $ M.fromList
            [ (Player T1 P1, object (PlayerState 100 255) player)
            , (Ball, object (BallState 80 (OriginRect 0 10)) ball)
            ]
        ) -< i

  returnA -< bg <> objs
