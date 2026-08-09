module Jam3Serious.Mail where

import Data.Map.Monoidal qualified as MM
import Jam3Serious.Prelude


onMail :: forall a. Typeable a => SF ObjInput (Event (Mail a))
onMail
  = arr
  $ maybeToEvent
  . listToMaybe
  . mapMaybe (traverse $ fromDynamic @a)
  . oi_inbox


on :: (Foldable f, Monoid m) => f a -> (a -> m) -> m
on = flip foldMap


send :: Typeable a => Name -> a -> MonoidalMap Name [Dynamic]
send n = MM.singleton n . pure . toDyn


respond :: Typeable a => a -> Mail b -> MonoidalMap Name [Dynamic]
respond a = flip send a . from

