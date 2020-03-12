{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Game.GameLevels.Generation.BSPGen
import System.Random
import Control.Monad.State

run :: RIO App ()
run = do
  logInfo $ displayShow $ runState (generateLevel param s) gen
  where
    param = GeneratorParameters 20 1.7 5
    s = Space (Coord 0 0) (Coord 100 100)
    gen = mkStdGen 42
  -- logInfo "We're inside the application!"
