{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Game.GameLevels.Generation.BSPGen
import System.Random
import Control.Monad.State
import Game.IO.TempRender
import Game.GameLevels.GenerateLevel

run :: RIO App ()
run = do
  foldMap (logInfo . displayShow) render
--  logInfo $ displayShow $ runState (generateLevel param s) gen
  where
    param = makeGeneratorParameters 20 1.7 5
    s = Space (Coord 0 0) (Coord 100 100)
    gen = mkStdGen 44
    (lvl, gen') = randomLevel s param gen
    render = renderLevel lvl

  -- logInfo "We're inside the application!"
