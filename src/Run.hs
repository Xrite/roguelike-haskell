{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Game.GameLevels.Generation.BSPGen
import System.Random
import Game.IO.TempRender
import Game.GameLevels.GenerateLevel

run :: RIO App ()
run = do
  foldMap (logInfo . displayShow) render
--  logInfo $ displayShow $ runState (generateLevel param s) gen
  where
    param = GeneratorParameters 10 1.7 5
    s = Space (Coord 0 0) (Coord 50 50)
    gen = mkStdGen 42
    (lvl, _) = randomLevel s param gen
    render = renderLevel lvl

  -- logInfo "We're inside the application!"
