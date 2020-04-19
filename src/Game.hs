{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Lens
import qualified Data.Map as Map (empty)
import Game.Environment
import Game.GameLevels.GameLevel
import Game.GameLevels.GenerateLevel (randomBSPGeneratedLevel, testGameLevel)
import Game.GameLevels.Generation.BSPGen (GeneratorParameters (..))
import qualified Game.GameLevels.Generation.GenerationUtil as GU
import Game.Item (createWeapon)
import Game.Modifiers.EffectAtom
import Game.Modifiers.EffectDesc (effectAtom)
import Game.Modifiers.UnitOpFactory (makeUnitOpFactory)
import Game.Unit.Action
import Game.Unit.Inventory (emptyInventory)
import Game.Unit.Mob
import Game.Unit.Player (Player, makePlayer)
import Game.Unit.Stats as Stats
import Game.Unit.TimedUnitOps (empty)
import Game.Unit.Unit (UnitData, createUnitData)
import System.Random (mkStdGen)
import UI.Descriptions.GameUIDesc
import qualified UI.Descriptions.ListMenuDesc as ListMenu
import UI.Keys as Keys
import UI.UI

data GameState
  = Game Environment
  | EndState

newtype MainMenuState = MainMenu (UI MainMenuState)

makeLenses ''GameState

instance HasUI GameState where
  getUI (Game env) = gameUI env
  getUI EndState = terminalUI

instance HasUI MainMenuState where
  getUI (MainMenu ui) = ui

gameUI :: Environment -> UI GameState
gameUI env = makeGameUI $
  do
    let (renderedMap, _) = runGameEnv renderEnvironment env
    setMap renderedMap
    setArrowPress arrowPress
    setKeyPress keyPress
  where
    arrowPress :: Arrows -> GameState -> AnyHasUI
    arrowPress Keys.Up (Game e) = packHasUI . Game . snd $ runGameEnv (evalAction (playerId env) moveUp) e
    arrowPress Keys.Down (Game e) = packHasUI . Game . snd $ runGameEnv (evalAction (playerId env) moveDown) e
    arrowPress Keys.Left (Game e) = packHasUI . Game . snd $ runGameEnv (evalAction (playerId env) moveLeft) e
    arrowPress Keys.Right (Game e) = packHasUI . Game . snd $ runGameEnv (evalAction (playerId env) moveRight) e
    arrowPress _ st = packHasUI st
    keyPress :: Keys.Keys -> GameState -> AnyHasUI
    keyPress (Keys.Letter 'q') (Game _) = packHasUI $ MainMenu mainMenuUI
    keyPress _ st = packHasUI st

mainMenuUI :: UI MainMenuState
mainMenuUI = makeListMenuUI $
  do
    ListMenu.setTitle "Main menu"
    ListMenu.addItem "random" (const (packHasUI $ Game $ randomEnvironment 42)) -- TODO use random generator or at least ask user to input a seed
    ListMenu.addItem "load level" (const . packHasUI $ MainMenu loadLvlMenuUI)
    ListMenu.addItem "test level" (const (packHasUI $ Game testEnvironment))
    ListMenu.addItem "quit" (const . packHasUI $ EndState)
    ListMenu.selectItem 0

-- TODO
loadLvlMenuUI :: UI MainMenuState
loadLvlMenuUI =
  makeListMenuUI $ do
    ListMenu.setTitle "Load level"
    ListMenu.addItem "level 1" undefined --(const (packHasUI $ Game testEnvironment))
    ListMenu.addItem "level 2" undefined --(const (packHasUI $ Game testEnvironment))
    ListMenu.addItem "back" (const . packHasUI $ MainMenu mainMenuUI)
    ListMenu.selectItem 0


randomEnvironment :: Int -> Environment
randomEnvironment seed =
  makeEnvironment
    ourPlayer
    []
    [lvl]
    (makeUnitOpFactory Map.empty)
  where
    lvl = fst $ randomBSPGeneratedLevel (GU.Space (GU.Coord 0 0) (GU.Coord 50 50)) (GeneratorParameters 10 1.7 5) $ mkStdGen seed
    startCoord = _entrance $ _lvlMap lvl
    ourPlayer = makeSomePlayer $ makeUnitData startCoord 'λ'

testEnvironment :: Environment
testEnvironment =
  makeEnvironment
    ourPlayer
    [ Mob (makeUnitData (14, 15) 'U') undefined,
      Mob (makeUnitData (4, 6) 'U') undefined,
      Mob (makeUnitData (5, 6) 'U') undefined
    ]
    [testGameLevel]
    (makeUnitOpFactory Map.empty)
  where
    ourPlayer = makeSomePlayer $ makeUnitData (7, 9) 'λ'

makeUnitData :: (Int, Int) -> Char -> UnitData
makeUnitData position render =
  createUnitData
    position
    0
    (Stats.Stats 10 10 10 1)
    empty
    emptyInventory
    (createWeapon "weapon" (effectAtom $ damage 5) 'A')
    render

makeSomePlayer :: UnitData -> Player
makeSomePlayer = makePlayer
