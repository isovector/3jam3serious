{-# LANGUAGE OverloadedLabels #-}

module Jam3Serious.Types
  ( module Jam3Serious.Types
  , SF
  ) where

import Data.Monoid
import GHC.Generics (Generic, Generically(..))
import Control.Arrow
import Data.Map (Map)
import Data.Dynamic
import Data.Map.Monoidal (MonoidalMap)
import FRP.Yampa
import SDL (V2)
import qualified SDL

instance Semigroup (Event a) where
  (<>) = lMerge

instance Monoid (Event a) where
  mempty = noEvent

deriving via (Ap (SF a) b) instance Semigroup b => Semigroup (SF a b)
deriving via (Ap (SF a) b) instance Monoid b => Monoid (SF a b)
deriving via (Ap (SF a) b) instance Num b => Num (SF a b)

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
  , oi_everyone :: Map Name ObjState
  }
  deriving stock (Generic)

data ObjOutput = ObjOtuput
  { oo_outbox :: MonoidalMap Name [Dynamic]
  , oo_output :: Output
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via Generically (ObjOutput)

data Name
  = Player1 | Player2
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
  deriving stock (Functor, Foldable, Traversable)
  deriving stock Generic
  deriving (Semigroup, Monoid) via Generically (ObjectMap a)


type Object = SF ObjInput (ObjOutput, ObjState)
type Obj a = SF (ObjInput, a) (ObjOutput, a)

data ObjState = ObjState
  {
  }

class ToObjState a where
  toObjState :: a -> ObjState

