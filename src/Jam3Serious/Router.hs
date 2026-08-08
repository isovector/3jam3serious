module Jam3Serious.Router where

import Jam3Serious.Prelude
import Data.Map qualified as M
import Data.Map.Monoidal qualified as MM
import Data.Monoid


router
    :: ObjectMap Object
    -> SF Input Output
router objs0 = loopPre mempty $
  router' objs0 >>> arr (foldMap (oo_output . fst) &&& fmap snd)


router'
    :: ObjectMap Object
    -> SF (Input, ObjectMap ObjState)
          (ObjectMap (ObjOutput, ObjState))
router' objs0 =
  pSwitch @ObjectMap
          @(Input, ObjectMap ObjState )
          @(ObjInput )
          @(ObjOutput , ObjState)
          @(ObjectMap Object -> ObjectMap Object)
    (\(i, outlast) om@(ObjectMap objs msgs) -> om
      { om_objects = flip M.mapWithKey objs $ \name sf ->
          (, sf) $ ObjInput
            { oi_input = i
            , oi_inbox = MM.findWithDefault mempty name msgs
            , oi_me = name
            , oi_everyone = om_objects outlast
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
        . fmap fst
        . snd
      ) >>> notYet
    )
    (\om f -> router' $ f $ om { om_messages = mempty })



decodeOutput :: Name -> ObjOutput -> Endo (ObjectMap Object)
decodeOutput n oo = mconcat
  [ flip foldMap (MM.toList $ oo_outbox oo) $ \(to, dyns) -> Endo $
      #om_messages <>~ MM.singleton to (fmap (Mail n) dyns)
  ]

object :: ToObjState a => a -> Obj a -> Object
object a0 obj = loopPre a0 $ obj >>> arr (\(oo, a) -> ((oo, toObjState a), a))

