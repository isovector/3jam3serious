module Jam3Serious.Router where

import Jam3Serious.Prelude
import Data.Map.Strict qualified as M
import Data.Map.Monoidal.Strict qualified as MM
import FRP.Yampa qualified as Y
import Data.Coerce


router
    :: ObjectMap Object
    -> Y.SF Input Output
router objs0 = Y.loopPre mempty $
  router' objs0 >>> arr (foldMap (oo_output . fst) &&& fmap snd)


router'
    :: ObjectMap Object
    -> Y.SF (Input, ObjectMap ObjState)
          (ObjectMap (ObjOutput, ObjState))
router' objs0 =
  Y.pSwitch @ObjectMap
          @(Input, ObjectMap ObjState)
          @(ObjInput, Global)
          @(ObjOutput, ObjState)
          @(ObjectMap Object -> ObjectMap Object)
    (\(i, outlast) om@(ObjectMap objs msgs) ->
      let g = Global (i_dt i) (om_objects outlast) in
      fmap (first (, g)) $
      om
        { om_objects = flip M.mapWithKey objs $ \name sf ->
            (, sf) $ ObjInput
              { oi_input = i
              , oi_inbox = MM.findWithDefault mempty name msgs
              , oi_me = name
              -- , oi_everyone = om_objects outlast
              }
        }
    )
    (coerce objs0)
    ( ( arr
        $ Event
        . appEndo
        . foldMap (uncurry decodeOutput)
        . M.assocs
        . om_objects
        . fmap fst
        . snd
      ) >>> Y.iPre NoEvent
    )
    (\om f -> router' $ f $ coerce $ om { om_messages = mempty })



decodeOutput :: Name -> ObjOutput -> Endo (ObjectMap Object)
decodeOutput n oo = mconcat
  [ flip foldMap (oo_commands oo) $ \case
      Spawn n' obj -> Endo $ #om_objects <>~ M.singleton n' obj
      Die -> Endo $ #om_objects %~ M.delete n
  , flip foldMap (MM.toList $ oo_outbox oo) $ \(to, dyns) -> Endo $
      #om_messages <>~ MM.singleton to (fmap (Mail n) dyns)
  ]

