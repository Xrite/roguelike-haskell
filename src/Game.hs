{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Lens
import Control.Monad.Except
import Data.Either (fromRight, rights)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Game.Environment
import Game.FileIO.FileIO (getLevelByName)
import Game.GameLevels.GameLevel
import Game.GameLevels.GenerateLevel (randomBSPGeneratedLevel, testGameLevel)
import Game.GameLevels.Generation.BSPGen (GeneratorParameters (..))
import qualified Game.GameLevels.Generation.GenerationUtil as GU
import Game.Item
import Game.Modifiers.EffectAtom
import Game.Modifiers.EffectDesc (effectAtom, effectTypical)
import Game.Modifiers.UnitOp
import Game.Modifiers.UnitOpFactory (makeUnitOpFactory)
import Game.Unit.Action
import Game.Unit.Control
import Game.Unit.Inventory
import Game.Unit.Stats as Stats
import Game.Unit.TimedUnitOps (empty)
import Game.Unit.Unit
import System.Random (getStdRandom, mkStdGen, random)
import qualified UI.Descriptions.GameUIDesc as GameUIDesc
import qualified UI.Descriptions.InventoryUIDesc as InventoryUI
import qualified UI.Descriptions.ListMenuDesc as ListMenuDesc
import qualified UI.Keys as Keys
import UI.UI

data GameState = Game Environment

newtype InventoryState = Inventory Environment

data EndState = EndState

newtype MainMenuState = MainMenu (UI IO MainMenuState)

makeLenses ''GameState

instance HasUI IO GameState where
  getUI (Game env) = gameUI env

instance HasUI IO InventoryState where
  getUI (Inventory env) = inventoryUI env

instance HasUI IO EndState where
  getUI EndState = terminalUI

instance HasUI IO MainMenuState where
  getUI (MainMenu ui) = ui

gameUI :: (Applicative m, HasUI m GameState, HasUI m MainMenuState, HasUI m InventoryState) => Environment -> UI m GameState
gameUI env = makeGameUIPure $
  do
    fst $ runGameEnv tryRender env
    fst $ runGameEnv tryGetStats env
    GameUIDesc.setArrowPress arrowPress
    GameUIDesc.setKeyPress keyPress
  where
    --    arrowPress :: (HasIOUI ma GameState) => Arrows -> GameState -> AnyHasIOUI ma
    arrowPress Keys.Up (Game e) = packHasIOUI . Game . snd $ runGameEnv (makeTurn moveUp) e
    arrowPress Keys.Down (Game e) = packHasIOUI . Game . snd $ runGameEnv (makeTurn moveDown) e
    arrowPress Keys.Left (Game e) = packHasIOUI . Game . snd $ runGameEnv (makeTurn moveLeft) e
    arrowPress Keys.Right (Game e) = packHasIOUI . Game . snd $ runGameEnv (makeTurn moveRight) e
    arrowPress _ st = packHasIOUI st
    --    keyPress :: Keys.Keys -> GameState -> AnyHasIOUI m
    keyPress (Keys.Letter 'q') (Game _) = packHasIOUI $ MainMenu mainMenuUI
    keyPress (Keys.Letter 'i') (Game e) = packHasIOUI $ Inventory e
    keyPress _ st = packHasIOUI st
    tryRender = do
      visible <- Set.fromList <$> getVisibleToPlayer
      seen <- Set.fromList <$> getSeenByPlayer
      cells <- getCells
      mobs <- getActiveMobs
      ms <- rights <$> traverse (\uid -> runExceptT $ (,) <$> getUnitPosition uid <*> getUnitPortrait uid) mobs
      playerUid <- getPlayer
      playerPosition <- runExceptT $ getUnitPosition playerUid
      playerPortrait <- runExceptT $ getUnitPortrait playerUid
      return $ do
        GameUIDesc.setMapTerrain cells
        GameUIDesc.setMapHasBeenSeenByPlayer (`Set.member` seen)
        GameUIDesc.setMapIsVisibleToPlayer (`Set.member` visible)
        GameUIDesc.setMapMobs ms
        fromRight (return ()) $ pure GameUIDesc.setMapPlayer <*> playerPosition <*> playerPortrait
    tryGetStats = do
      uid <- getPlayer
      eStats <- runExceptT $ getUnitStats uid
      eLevellingStats <- runExceptT $ getLevellingStats uid
      return $ fromRight (return ()) $ do
        stats <- eStats
        levellingStats <- eLevellingStats
        return $
          GameUIDesc.setStats
            [ ("Health", show (stats ^. health)),
              ("Attack power", show (stats ^. attackPower)),
              ("Shield", show (stats ^. shield)),
              ("Level", show (stats ^. level)),
              ("Experience", show (levellingStats ^. experience)),
              ("Skill points", show (levellingStats ^. skillPoints))
            ]

mainMenuUI :: UI IO MainMenuState
mainMenuUI = makeListMenuUI $
  do
    ListMenuDesc.setTitle "Main menu"
    ListMenuDesc.addItem "random" (const (packHasIOUI . Game . randomEnvironment <$> getStdRandom random)) -- TODO use random generator or at least ask user to input a seed
    ListMenuDesc.addItemPure "load level" (const $ packHasIOUI $ MainMenu loadLvlMenuUI)
    ListMenuDesc.addItemPure "test level" (const (packHasIOUI $ Game testEnvironment))
    ListMenuDesc.addItemPure "quit" (const . packHasIOUI $ EndState)
    ListMenuDesc.selectItem 0

inventoryUI :: (Applicative m, HasUI m InventoryState, HasUI m GameState) => Environment -> UI m InventoryState
inventoryUI env = makeInventoryUIPure $
  do
    InventoryUI.setItems $ map name (inv ^. items)
    InventoryUI.setSlots
      [ ("Head", defaultSlot (view wearableName) (inv ^. wearableSlots . headSlot)),
        ("Chest", defaultSlot (view wearableName) (inv ^. wearableSlots . chestSlot)),
        ("Legs", defaultSlot (view wearableName) (inv ^. wearableSlots . legsSlot)),
        ("Hand", defaultSlot (view weaponName) (inv ^. weaponSlots . hand))
      ]
    InventoryUI.setOnSlotSelected $ clickSlot
    InventoryUI.setOnItemSelected $ clickItem
    InventoryUI.setOnClosed $ close
    InventoryUI.selectItem 0
  where
    (inv, _) = runGameEnv getPlayerInventory env
    defaultSlot = maybe "free"
    clickSlot i (Inventory e)
      | i == 0 = packHasIOUI . Inventory . snd $ runGameEnv (setPlayerInventory (freeHeadSlot inv)) env
      | i == 1 = packHasIOUI . Inventory . snd $ runGameEnv (setPlayerInventory (freeChestSlot inv)) env
      | i == 2 = packHasIOUI . Inventory . snd $ runGameEnv (setPlayerInventory (freeLegsSlot inv)) env
      | i == 3 = packHasIOUI . Inventory . snd $ runGameEnv (setPlayerInventory (freeHandSlot inv)) env
      | otherwise = packHasIOUI $ Inventory e
    clickItem i (Inventory e) = let item = (inv ^. items) !! i in
      case tryFillSlot inv item of
        Left _ -> packHasIOUI $ Inventory e
        Right newInv -> packHasIOUI . Inventory . snd $ runGameEnv (setPlayerInventory newInv) env
    close (Inventory e) = packHasIOUI $ Game e

loadLevel :: String -> IO GameLevel
loadLevel name = do
  levelEither <- getLevelByName name
  let level1 = fromRight testGameLevel levelEither
  return level1

loadLvlMenuUI :: UI IO MainMenuState
loadLvlMenuUI =
  makeListMenuUI $ do
    ListMenuDesc.setTitle "Load level"
    ListMenuDesc.addItem "level 1" (const $ packHasIOUI . Game . testEnvironmentWithLevel <$> loadLevel "Level_1")
    ListMenuDesc.addItem "level 2" (const $ packHasIOUI . Game . testEnvironmentWithLevel <$> loadLevel "Level_2")
    ListMenuDesc.addItem "level 3" (const $ packHasIOUI . Game . testEnvironmentWithLevel <$> loadLevel "Level_3")
    ListMenuDesc.addItemPure "back" (const $ packHasIOUI $ MainMenu mainMenuUI)
    ListMenuDesc.selectItem 0

testEnvironmentWithLevel :: GameLevel -> Environment
testEnvironmentWithLevel level =
  makeEnvironment
    ourPlayer
    [makeMob (makeUnitData (3, 3) 'U') Aggressive]
    [level]
    (makeUnitOpFactory Map.empty)
  where
    ourPlayer = makeSomePlayer $ makeUnitData (level ^. lvlMap . entrance) 'λ'

randomEnvironment :: Int -> Environment
randomEnvironment seed =
  makeEnvironment
    ourPlayer
    []
    [lvl]
    (makeUnitOpFactory $ Map.singleton "confuse" $ setTimedUnitOp 10 (const $ setEffect confuse))
  where
    lvl = fst $ randomBSPGeneratedLevel (GU.Space (GU.Coord 0 0) (GU.Coord 50 50)) (GeneratorParameters 10 1.7 5) $ mkStdGen seed
    startCoord = _entrance $ _lvlMap lvl
    ourPlayer = makeSomePlayer $ makeUnitData startCoord 'λ'

testEnvironment :: Environment
testEnvironment =
  makeEnvironment
    ourPlayer
    [ makeMob (makeUnitData (14, 15) 'U') Aggressive,
      makeMob (makeUnitData (4, 6) 'U') (Passive (4, 6)),
      makeMob (makeUnitData (5, 6) 'U') Avoiding
    ]
    [testGameLevel]
    (makeUnitOpFactory $ Map.singleton "confuse" $ setTimedUnitOp 10 (const $ setEffect confuse))
  where
    ourPlayer = makeSomePlayer $ makeUnitData (7, 9) 'λ'

makeUnitData :: (Int, Int) -> Char -> UnitData
makeUnitData position render =
  createUnitData
    position
    0
    (Stats.Stats 10 10 10 1)
    Game.Unit.TimedUnitOps.empty
    emptyInventory
    (createWeapon "weapon" (effectAtom (damage 1) >> effectTypical "confuse") 'A')
    render

makeSomePlayer :: UnitData -> Player
makeSomePlayer = makePlayer

makeTurn :: Action -> GameEnv ()
makeTurn playerAction = do
  player <- getPlayer
  _ <- runExceptT (evalAction player playerAction)
  mobs <- getActiveMobs
  _ <- runExceptT $ traverse (\u -> getAction u >>= evalAction u) mobs
  units <- getActiveUnits
  _ <- runExceptT $ traverse (`affectUnit` tickTimedEffects) units
  return ()
