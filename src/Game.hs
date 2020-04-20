{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Game where

import Control.Lens
import qualified Data.Map as Map (empty)
import Data.Functor (($>))
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
import Game.Unit.Stats as Stats
import Game.Unit.TimedUnitOps (empty, addUnitOp)
import Game.Unit.Unit
import System.Random (mkStdGen)
import UI.Descriptions.GameUIDesc
import qualified UI.Descriptions.ListMenuDesc as ListMenu
import UI.Keys as Keys
import UI.UI
import Game.FileIO.FileIO (getLevelByName)
import Data.Either (fromRight)
import Game.Modifiers.UnitOp (setEffect)

data GameState
  = Game Environment
  | EndState

data MainMenuState = MainMenu (UI IO MainMenuState)

makeLenses ''GameState

instance HasUI IO GameState where
  getUI (Game env) = gameUI env
  getUI EndState = terminalUI

instance HasUI IO MainMenuState where
  getUI (MainMenu ui) = ui

gameUI :: (Applicative m, HasUI m GameState, HasUI m MainMenuState) => Environment -> UI m GameState
gameUI env = makeGameUIPure $
  do
    let (renderedMap, _) = runGameEnv renderEnvironment env
    setMap renderedMap
    setArrowPress arrowPress
    setKeyPress keyPress
  where
--    arrowPress :: (HasIOUI ma GameState) => Arrows -> GameState -> AnyHasIOUI ma
    arrowPress Keys.Up (Game e) = packHasIOUI . Game . snd $ runGameEnv (evalAction (playerId env) moveUp) e
    arrowPress Keys.Down (Game e) = packHasIOUI . Game . snd $ runGameEnv (evalAction (playerId env) moveDown) e
    arrowPress Keys.Left (Game e) = packHasIOUI . Game . snd $ runGameEnv (evalAction (playerId env) moveLeft) e
    arrowPress Keys.Right (Game e) = packHasIOUI . Game . snd $ runGameEnv (evalAction (playerId env) moveRight) e
    arrowPress _ st = packHasIOUI st
--    keyPress :: Keys.Keys -> GameState -> AnyHasIOUI m
    keyPress (Keys.Letter 'q') (Game _) = packHasIOUI $ MainMenu mainMenuUI
    keyPress _ st = packHasIOUI st

mainMenuUI :: UI IO MainMenuState
mainMenuUI = makeListMenuUI $
  do
    ListMenu.setTitle "Main menu"
    ListMenu.addItemPure "random" (const (packHasIOUI $ Game $ randomEnvironment 42)) -- TODO use random generator or at least ask user to input a seed
    ListMenu.addItemPure "load level" (const $ packHasIOUI $ MainMenu loadLvlMenuUI)
    ListMenu.addItemPure "test level" (const (packHasIOUI $ Game testEnvironment))
    ListMenu.addItem "test level & write to HI.txt" (const (appendFile "HI.txt" "x\n" $> packHasIOUI (Game testEnvironment)))
    ListMenu.addItemPure "quit" (const . packHasIOUI $ EndState)
    ListMenu.selectItem 0

loadLevel :: IO GameLevel
loadLevel = do
  levelEither <- getLevelByName "Level1"
--  level1 <- fromEither levelEither
  let level1 = fromRight testGameLevel levelEither
  return level1

loadLvlMenuUI :: UI IO MainMenuState
loadLvlMenuUI =
  makeListMenuUI $ do
    ListMenu.setTitle "Load level"
    ListMenu.addItem "level 1" (const $ packHasIOUI . Game . testEnvironmentWithLevel <$> loadLevel)
    ListMenu.addItemPure "back" (const $ packHasIOUI $ MainMenu mainMenuUI)
    ListMenu.selectItem 0

testEnvironmentWithLevel :: GameLevel -> Environment
testEnvironmentWithLevel level =
  makeEnvironment
    ourPlayer
    [makeMob (makeUnitData (1, 1) 'U')]
    [level]
    (makeUnitOpFactory Map.empty)
  where
    ourPlayer = makeSomePlayer $ makeUnitData (2, 2) 'λ'

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
    [ makeMob (makeUnitData (14, 15) 'U'),
      makeMob (makeUnitData (4, 6) 'U'),
      makeMob (makeUnitData (5, 6) 'U')
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
    (addUnitOp 10 (const $ setEffect $ confuse) empty)
    emptyInventory
    (createWeapon "weapon" (effectAtom $ damage 5) 'A')
    render

makeSomePlayer :: UnitData -> Player
makeSomePlayer = makePlayer
