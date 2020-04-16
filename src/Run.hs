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
import UI.BrickUI (app)
import Game.GameLevels.Generation.GenerationUtil

run :: RIO App ()
run = do
--  foldMap (logInfo . displayShow) render
--  logInfo $ displayShow $ runState (generateLevel param s) gen
  liftIO $ void $ defaultMain app (MainMenu mainMenuUI, mainMenuUI )
  where
    param = GeneratorParameters 10 1.7 5
    s = Space (Coord 0 0) (Coord 50 50)
    gen = mkStdGen 42
    (lvl, _) = randomBSPGeneratedLevel s param gen
--    render = renderLevel lvl

  -- logInfo "We're inside the application!"
