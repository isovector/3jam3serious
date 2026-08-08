{-# LANGUAGE OverloadedLabels #-}

module Jam3Serious.Types
  ( module Jam3Serious.Types
  , SF
  ) where

import GHC.Generics (Generic)
import Control.Arrow
import Data.Map (Map)
import Data.Dynamic
import Data.Map.Monoidal (MonoidalMap)
import FRP.Yampa
import SDL (V2)
import qualified SDL


data Input = Input
  { i_keyboard :: SDL.Scancode -> Bool
  , i_mouse :: SDL.MouseButton -> Bool
  , i_mousepos :: SDL.Point SDL.V2 Int
  , i_dt :: DTime
  }
  deriving stock Generic


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



data ObjInput = ObjInput
  { oi_input :: Input
  , oi_inbox :: [Mail]
  , oi_me :: Name
  }
  deriving stock Generic

data ObjOutput = ObjOtuput
  { oo_outbox :: MonoidalMap Name [Dynamic]
  }
  deriving stock Generic

data Name
  deriving stock (Eq, Ord)

data Mail = Mail
  { from :: Name
  , message :: Dynamic
  }
  deriving stock Generic

data ObjectMap a = ObjectMap
  { om_objects  :: Map Name a
  , om_messages :: MonoidalMap Name [Mail]
  }
  deriving stock Functor
  deriving stock Generic


type Object = SF ObjInput ObjOutput

