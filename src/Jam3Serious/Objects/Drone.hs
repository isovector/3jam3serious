{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Jam3Serious.Objects.Drone where

import Jam3Serious.Prelude
import Jam3Serious.Objects.Player


opponentPos :: SF ObjInput (V3 Double)
opponentPos = proc oi -> do
  who
    <- friend
         (\(Player t ix) n o ->
            bool Nothing (Just o) $ n == Player (otherTeam t) ix)
    -< oi_me oi
  returnA -< fromMaybe 0 $ os_pos =<< who

defense :: SF (ObjInput, PlayerState) Controller
defense = proc (oi, ps) -> do
  opos <- opponentPos -< oi
  npos <- netPos <<< myTeam -< oi
  let wanted = (npos - opos) / 2 + opos
  let pos = ps_pos ps

  returnA -< Controller
    { c_dir = normalize $ (wanted - pos) ^. _xy
    , c_jump = NoEvent
    , c_shoot = NoEvent
    , c_pass = NoEvent
    , c_run = False
    }
