module Jam3Serious.Yampa
  ( runSF
  ) where

import Control.Monad
import Data.Foldable
import Data.IORef
import Data.Map.Monoidal.Strict qualified as MM
import Data.Time.Clock.System (SystemTime, getSystemTime, systemSeconds, systemNanoseconds)
import Euterpea.IO.MIDI.MidiIO (unsafeOutputID)
import FRP.Yampa (DTime, reactimate)
import FRP.Yampa qualified as Y
import Jam3Serious.Types
import qualified SDL



floatSeconds :: SystemTime -> Double
floatSeconds t
  = fromIntegral (systemSeconds t)
  + fromIntegral (systemNanoseconds t) / 1e9


runSF :: SDL.Renderer -> Gfx -> Soundbank -> Y.SF Input Output -> IO ()
runSF renderer gfx audio sf = do
  t0 <- fmap floatSeconds getSystemTime
  time_ref <- newIORef t0

  reactimate
    (sampleInput 0)
    (\_ -> do
      t <- readIORef time_ref
      t' <- fmap floatSeconds getSystemTime
      writeIORef time_ref t'

      let dt = t' - t
      when (dt > 0.017) $ putStrLn $ "Slow frame! dt=" <> show dt
      fmap ((dt,) . Just) $ sampleInput dt
    )
    (\_ out -> do
      fold (fmap snd $ MM.toList $ runOutput out) (unsafeOutputID 2) renderer gfx audio
      SDL.present renderer
      fmap (any isQuitEvent) SDL.pollEvents
    )
    sf


sampleInput :: DTime -> IO Input
sampleInput dt = do
  kbState     <- SDL.getKeyboardState
  mbState     <- SDL.getMouseButtons
  SDL.P pos   <- SDL.getAbsoluteMouseLocation
  let pos' = SDL.P (fmap fromIntegral pos)
  pure Input
    { i_keyboard = Keyboard kbState
    , i_mouse    = Mouse mbState pos'
    , i_dt       = dt
    }


isQuitEvent :: SDL.Event -> Bool
isQuitEvent event =
  case SDL.eventPayload event of
    SDL.QuitEvent -> True
    SDL.KeyboardEvent ke ->
      SDL.keyboardEventKeyMotion ke == SDL.Pressed &&
      SDL.keysymKeycode (SDL.keyboardEventKeysym ke) == SDL.KeycodeEscape
    _ -> False

