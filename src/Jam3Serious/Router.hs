module Jam3Serious.Router where

import Control.Arrow
import Control.Lens ((<>~))
import Data.Generics.Labels ()
import Data.Map qualified as M
import Data.Map.Monoidal qualified as MM
import Data.Maybe (fromMaybe)
import Data.Monoid
import FRP.Yampa
import Jam3Serious.Types


router
    :: ObjectMap Object
    -> SF Input (ObjectMap ObjOutput)
router objs0 =
  pSwitch @ObjectMap
          @Input
          @ObjInput
          @ObjOutput
          @(ObjectMap Object -> ObjectMap Object)
    (\i om@(ObjectMap objs msgs) -> om
      { om_objects = flip M.mapWithKey objs $ \name sf ->
          (, sf) $ ObjInput
            { oi_input = i
            , oi_inbox = fromMaybe mempty $ MM.lookup name msgs
            , oi_me = name
            }
      }
    )
    objs0
    ( ( arr
        $ Event
        . appEndo
        . foldMap (uncurry decodeOutput)
        . M.assocs
        . om_objects
        . snd
      ) >>> notYet
    )
    (\om f -> router $ (f om) { om_messages = mempty })



decodeOutput :: Name -> ObjOutput -> Endo (ObjectMap Object)
decodeOutput n oo = mconcat
  [ flip foldMap (MM.toList $ oo_outbox oo) $ \(to, dyns) -> Endo $
      #om_messages <>~ MM.singleton to (fmap (Mail n) dyns)
  ]
