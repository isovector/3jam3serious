module Jam3Serious.Types
  ( module Jam3Serious.Types
  , SF
  ) where

import FRP.Yampa (SF, DTime, arr)
import SDL (V2)
import qualified SDL


data Input = Input
  { i_keyboard :: SDL.Scancode -> Bool
  , i_mouse :: SDL.MouseButton -> Bool
  , i_mousepos :: SDL.Point SDL.V2 Int
  , i_dt :: DTime
  }


newtype Output = Output
  { runOutput :: SDL.Renderer -> IO ()
  }
  deriving newtype (Semigroup, Monoid)


raw :: (SDL.Renderer -> IO ()) -> Output
raw = Output


mousePos :: SF Input (V2 Int)
mousePos = arr $ SDL.unP . i_mousepos


mouseBtn :: SDL.MouseButton -> SF Input Bool
mouseBtn = arr . flip i_mouse


keyboard :: SDL.Scancode -> SF Input Bool
keyboard = arr . flip i_keyboard

