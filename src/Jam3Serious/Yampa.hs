module Jam3Serious.Yampa
  ( runSF
  ) where

import Data.Foldable
import Data.Map.Monoidal.Strict qualified as MM
import Control.Monad
import Data.IORef
import Data.Time.Clock.System (SystemTime, getSystemTime, systemSeconds, systemNanoseconds)
import FRP.Yampa (DTime, reactimate)
import Jam3Serious.Types
import qualified SDL


floatSeconds :: SystemTime -> Double
floatSeconds t
  = fromIntegral (systemSeconds t)
  + fromIntegral (systemNanoseconds t) / 1e9


runSF :: SDL.Renderer -> SF Input Output -> IO ()
runSF renderer sf = do
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
      fold (fmap snd $ MM.toList $ runOutput out) renderer
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
    { i_keyboard = kbState
    , i_mouse    = mbState
    , i_mousepos = pos'
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

