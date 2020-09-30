{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Game.GameLevels.Generation.BSPGen
import System.Random
import Game.IO.TempRender
import Game.GameLevels.GenerateLevel
import Game.GameLevels.Generation.BSPGen
import Brick.Main (customMain)
import Game
import UI.BrickUI (app, packUIState)
import Game.GameLevels.Generation.GenerationUtil
import Brick.BChan
import qualified Networking.Testmain as Testmain
import Game.Multiplayer.Server (runServer)
import AppOptions

-- It was here
import qualified Graphics.Vty as V

run :: RIO App ()
run = do
  args <- appOptions <$> ask
  runWithOptions args


runWithOptions :: AppOptions -> RIO App ()
runWithOptions (ServerOptions port seed) = do
  resSeed <-
    if seed /= 0
      then return seed
      else liftIO (getStdRandom random)
  liftIO $ runServer resSeed port

runWithOptions LocalOptions = do
  chan <- liftIO $ newBChan 1000
  let initialState = packUIState (MainMenu (mainMenuUI chan)) (mainMenuUI chan)
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- liftIO buildVty
  liftIO $ void $ customMain initialVty buildVty (Just chan) app initialState
  where
    param = GeneratorParameters 10 1.7 5
    s = Space (Coord 0 0) (Coord 50 50)
    gen = mkStdGen 42
    (lvl, _) = randomBSPGeneratedLevel s param gen


-- import qualified Networking.Testmain as Testmain

{- run :: RIO App ()
run = liftIO Testmain.main -}
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
