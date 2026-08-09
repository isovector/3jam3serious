{-# OPTIONS_GHC -Wno-orphans  #-}

module Jam3Serious.Types
  ( module Jam3Serious.Types
  , V2(..)
  , V3(..)
  , SF
  ) where

import Control.Arrow
import Data.Dynamic
import Data.Map (Map)
import Data.Map.Monoidal (MonoidalMap)
import Data.Monoid
import Data.Profunctor
import FRP.Yampa
import GHC.Generics (Generic, Generic1, Generically(..), Generically1(..))
import SDL (V2(..), V3(..))
import qualified SDL

deriving via (Ap Event a) instance Semigroup a => Semigroup (Event a)
deriving via (Ap Event a) instance Monoid a => Monoid (Event a)
deriving via (Ap (SF a) b) instance Semigroup b => Semigroup (SF a b)
deriving via (Ap (SF a) b) instance Monoid b => Monoid (SF a b)
deriving via (Ap (SF a) b) instance Num b => Num (SF a b)
deriving via (Ap ((->) a) b) instance Num b => Num (a -> b)
deriving stock instance Foldable Event
deriving stock instance Traversable Event

instance Profunctor SF where
  dimap f g sf = arr f >>> sf >>> arr g


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





data ObjInput = ObjInput
  { oi_input :: Input
  , oi_inbox :: [Mail Dynamic]
  , oi_me :: Name
  , oi_everyone :: Map Name ObjState
  }
  deriving stock (Generic)

data ObjOutput = ObjOtuput
  { oo_outbox :: MonoidalMap Name [Dynamic]
  , oo_commands :: [Command]
  , oo_output :: Output
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via Generically (ObjOutput)

data Command
  = Spawn Name Object
  | Die
  deriving stock (Generic)


data PlayerNum = P1 | P2
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

otherPlayerNum :: PlayerNum -> PlayerNum
otherPlayerNum P1 = P2
otherPlayerNum P2 = P1

data Team = T1 | T2
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

data Name
  = Player Team PlayerNum | Ball
  deriving stock (Eq, Ord, Show, Generic)

data Mail a = Mail
  { from :: Name
  , message :: a
  }
  deriving stock (Generic, Functor, Foldable, Traversable)

data ObjectMap a = ObjectMap
  { om_objects  :: Map Name a
  , om_messages :: MonoidalMap Name [Mail Dynamic]
  }
  deriving stock (Functor, Foldable, Traversable)
  deriving stock Generic
  deriving (Semigroup, Monoid) via Generically (ObjectMap a)


type Object = SF ObjInput (ObjOutput, ObjState)
type Obj a = SF (ObjInput, a) (ObjOutput, a)

data ObjState = ObjState
  { os_pos :: Maybe (V3 Double)
  , os_collision :: Maybe (Capsule Double)
  }

class ToObjState a where
  toObjState :: a -> ObjState

data OriginRect aff = OriginRect
  { orect_size   :: V2 aff
  , orect_offset :: V2 aff
  }
  deriving (Eq, Ord, Show, Functor, Generic)

data Capsule a = Capsule
  { c_top :: a
    -- ^ Height above 'c_pos'
  , c_bottom :: a
    -- ^ Height below 'c_pos'
  , c_radius :: a
    -- ^ Radius of the capsule
  , c_pos :: V3 a
  }
  deriving stock (Generic, Generic1, Functor, Foldable, Traversable)
  deriving Applicative via Generically1 Capsule

