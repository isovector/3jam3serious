module Lib (main) where

import Control.Exception (bracket, bracket_)
import Data.Atlas
import Data.Map qualified as M
import Data.Text (pack)
import Euterpea.IO.MIDI.MidiIO (initializeMidi , terminateMidi)
import FRP.Yampa qualified as Y
import Jam3Serious.Audio
import Jam3Serious.Objects.Ball
import Jam3Serious.Objects.Basket
import Jam3Serious.Objects.Camera
import Jam3Serious.Objects.Court
import Jam3Serious.Objects.Game
import Jam3Serious.Objects.Player
import Jam3Serious.Objects.Rim
import Jam3Serious.Prelude
import Jam3Serious.Router
import Jam3Serious.Yampa
import SDL qualified
import Sound.ALUT qualified as ALUT


windowTitle :: String
windowTitle = "3jam3serious"


main :: IO ()
main
  = ALUT.withProgNameAndArgs ALUT.runALUT $ \_ _ ->
    bracket_ (initializeMidi >> SDL.initializeAll) (terminateMidi >> SDL.quit) $ do
  window <- SDL.createWindow
    (pack windowTitle)
    SDL.defaultWindow
      { SDL.windowInitialSize = SDL.V2
          windowWidth
          windowHeight
      }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  audio <-
    Soundbank
      <$> loadAudio "res/3pts.wav"
  gfx <-
    Gfx
      <$> loadAtlas renderer "res/player.json"
      <*> loadAtlas renderer "res/numbers.json"
  bracket (pure (window, renderer))
          (\(w, r) -> SDL.destroyRenderer r >> SDL.destroyWindow w)
          (\_      -> runSF renderer gfx audio appSF)


appSF :: Y.SF Input Output
appSF = proc i -> do
  let bg = flip raw DDCourt $ \renderer _ _ -> do
        SDL.rendererDrawColor renderer SDL.$= V4 100 149 237 255
        SDL.clear renderer
  objs
    <- ystackFrame $ router
        ( flip ObjectMap mempty
        $ M.fromList $
            [ (Player T1 $ PlayerNum 0, object (PlayerState (V3 (-1) 0 0) False) $ player playerController)
            -- , (Player T1 $ PlayerNum 1, object (PlayerState (V3 (-1) 0 0) False) $ player defense)
            , (Court, object () court)
            -- , (Player T2 $ PlayerNum 0, object (PlayerState (V3 1 0 0) False) $ player defense)
            -- , (Player T2 $ PlayerNum 1, object (PlayerState (V3 1 0 0) False) $ player defense)
            , (Ball, object (BallState (V3 0 0 2) 0) ball)
            , (Basket T1, object (V3 (-12) 0 4) $ basket T2 $ V3 1 0 0 )
            , (Basket T2, object (V3 12 0 4) $ basket T1 $ V3 (-1) 0 0)
            , (Camera, object (CameraState (V3 0 0 2) (50 * 50) 20 Ball) camera)
            , (Game, object () game)
            ] <> do
              (t, o) <- zip [T1, T2] [V3 (-12) 0 4, V3 12 0 4]
              (i, r) <- zip [0..] $ mkRim 8 o
              pure (Rim t i, object r rim)

        ) -< i

  returnA -< bg <> objs
