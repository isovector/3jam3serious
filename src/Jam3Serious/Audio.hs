module Jam3Serious.Audio
  ( loadAudio
  , ALUT.Source
  ) where

import qualified Sound.ALUT as ALUT


loadAudio :: FilePath -> IO (ALUT.Source)
loadAudio fileName = do
  buf <- ALUT.createBuffer (ALUT.File fileName)
  src <- ALUT.genObjectName
  ALUT.loopingMode src ALUT.$= ALUT.OneShot
  ALUT.buffer src ALUT.$= Just buf
  pure src

