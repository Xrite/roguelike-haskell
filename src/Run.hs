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
import Brick.BChan
import qualified Networking.Testmain as Testmain

run :: RIO App ()
run = liftIO Testmain.main
--run = do
--  chan <- liftIO $ newBChan 10
--  liftIO $ void $ defaultMain app (packUIState (MainMenu (mainMenuUI chan)) (mainMenuUI chan))
--  where
--    param = GeneratorParameters 10 1.7 5
--    s = Space (Coord 0 0) (Coord 50 50)
--    gen = mkStdGen 42
--    (lvl, _) = randomBSPGeneratedLevel s param gen

--    render = renderLevel lvl

  -- logInfo "We're inside the application!"
