{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Game.GameLevels.Generation.BSPGen
import System.Random
import Game.IO.TempRender
import Game.GameLevels.GenerateLevel
import Game.GameLevels.Generation.BSPGen
import Brick.Main (defaultMain)
import Game
import UI.BrickUI (app, packUIState)
import Game.GameLevels.Generation.GenerationUtil
import Game.FileIO.FileIO

run :: RIO App ()
run = do
  res <- liftIO loadResources
-- trying to load map from disk:
--  logInfo $ displayShow $ readMap "#...\n##..\n###.\n####"
--  let level = res ^. savedLevels
--  logInfo $ displayShow level
  liftIO $ void $ defaultMain app $ packUIState (MainMenu mainMenuUI res) mainMenuUI
  where
    param = GeneratorParameters 10 1.7 5
    s = Space (Coord 0 0) (Coord 50 50)
    gen = mkStdGen 42
    (lvl, _) = randomBSPGeneratedLevel s param gen
--    render = renderLevel lvl

  -- logInfo "We're inside the application!"
