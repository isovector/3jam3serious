module Jam3Serious.Yampa
  ( runSF
  ) where

import Control.Monad
import Data.Function (fix)
import Data.IORef
import FRP.Yampa (DTime)
import FRP.Yampa.Simulation (evalAtZero, evalAt)
import Jam3Serious.Types
import qualified SDL


runSF :: SDL.Renderer -> SF Input Output -> IO ()
runSF renderer sf = do
  input0 <- sampleInput 0
  let (out0, future0) = evalAtZero sf input0
  runOutput out0 renderer
  SDL.present renderer

  ref <- newIORef future0
  t0  <- SDL.ticks

  flip fix t0 $ \loop prevTicks -> do
    events <- SDL.pollEvents
    let quit = any isQuitEvent events

    nowTicks <- SDL.ticks
    let dt :: DTime
        dt = fromIntegral (nowTicks - prevTicks) / 1000.0

    future <- readIORef ref
    input  <- sampleInput dt
    let (out, future') = evalAt future dt input
    writeIORef ref future'

    runOutput out renderer
    SDL.present renderer
    unless quit $ loop nowTicks


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

