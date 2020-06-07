{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Game where

import Debug.Trace
import Brick.BChan
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
import Game.Modifiers.UnitOpFactory (makeUnitOpFactory, UnitOpFactory)
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
import Game.FileIO.SaveGame
import Game.Transaction (Transaction)
import qualified Game.Transaction as Transaction
import Game.GameLevels.MapCell (renderCell)
import Game.Position

data CustomEvent 
--  = UpdateEnvUsingTransaction Transaction
--  | UpdateEnvUsingMemento EnvMemento
  = UpdateEnvironment Environment

data Handle =
  Handle {
    _handleDeath :: Environment -> IO (),
    _handleQuitGame :: Environment -> IO (),
    _handleAction :: PlayerId -> Action -> Environment -> IO Environment,
    _handleClickSlot :: PlayerId -> Int -> Environment -> IO Environment,
    _handleClickItem :: PlayerId -> Int -> Environment -> IO Environment
  }

data GameState 
  = Game {
    _gamePlayerId :: PlayerId,
    _gameEnv :: Environment,
    _gameHandle :: Handle,
    _gameBChan :: BChan CustomEvent
  }

data InventoryState 
  = Inventory {
    _inventoryPlayerId :: PlayerId,
    _inventoryEnv :: Environment,
    _inventoryHandle :: Handle,
    _inventoryBChan :: BChan CustomEvent
  }

data EndState = EndState

newtype MainMenuState = MainMenu (UI IO MainMenuState CustomEvent)

makeLenses ''Handle
makeLenses ''GameState
makeLenses ''InventoryState

instance HasUI IO GameState CustomEvent where
  getUI gs = gameUI (gs ^. gameEnv) (gs ^. gamePlayerId)

instance HasUI IO InventoryState CustomEvent where
  getUI is = inventoryUI (is ^. inventoryEnv) (is ^. inventoryPlayerId)

instance HasUI IO EndState CustomEvent where
  getUI EndState = terminalUI

instance HasUI IO MainMenuState CustomEvent where
  getUI (MainMenu ui) = ui

arrowsToAction :: Keys.Arrows -> Action
arrowsToAction Keys.Up = Move Zero Positive
arrowsToAction Keys.Down = Move Zero Negative
arrowsToAction Keys.Left = Move Negative Zero
arrowsToAction Keys.Right = Move Positive Zero
arrowsToAction Keys.UpLeft = Move Negative Positive
arrowsToAction Keys.UpRight = Move Positive Positive
arrowsToAction Keys.DownLeft = Move Negative Negative
arrowsToAction Keys.DownRight = Move Positive Negative
arrowsToAction Keys.Center = Move Zero Zero

keysToAction :: Keys.Keys -> Maybe Action
keysToAction (Keys.Letter '8') = Just $ Move Zero Positive
keysToAction (Keys.Letter '2') = Just $ Move Zero Negative
keysToAction (Keys.Letter '4') = Just $ Move Negative Zero
keysToAction (Keys.Letter '6') = Just $ Move Positive Zero
keysToAction (Keys.Letter '7') = Just $ Move Negative Positive
keysToAction (Keys.Letter '9') = Just $ Move Positive Positive
keysToAction (Keys.Letter '1') = Just $ Move Negative Negative
keysToAction (Keys.Letter '3') = Just $ Move Positive Negative
keysToAction (Keys.Letter '5') = Just $ Move Zero Zero
keysToAction (Keys.Letter 'j') = Just $ Move Zero Positive
keysToAction (Keys.Letter 'k') = Just $ Move Zero Negative
keysToAction (Keys.Letter 'h') = Just $ Move Negative Zero
keysToAction (Keys.Letter 'l') = Just $ Move Positive Zero
keysToAction (Keys.Letter 'y') = Just $ Move Negative Positive
keysToAction (Keys.Letter 'u') = Just $ Move Positive Positive
keysToAction (Keys.Letter 'b') = Just $ Move Negative Negative
keysToAction (Keys.Letter 'n') = Just $ Move Positive Negative
keysToAction (Keys.Letter '.') = Just $ Move Zero Zero
keysToAction _ = Nothing

--gameUI :: (Applicative m, HasUI m GameState, HasUI m MainMenuState) => Environment -> UI m GameState
gameUI :: Environment -> PlayerId -> UI IO GameState CustomEvent
gameUI env pid = makeGameUI $
  do
    traceM "start drawing game UI"
    fromRight (error "failed to render map") renderMap
    fromRight (error "failed to render stats") renderStats
    GameUIDesc.setArrowPress arrowPress
    GameUIDesc.setKeyPress keyPress
    GameUIDesc.setCustomEventHandler customEvent
  where
    arrowPress :: Keys.Arrows -> GameState -> IO (AnyHasUI IO CustomEvent)
    arrowPress arrow gs = do
      let env = (gs ^. gameEnv)
      let pid = (gs ^. gamePlayerId)
      let act = arrowsToAction arrow
      let chan = gs ^. gameBChan
      env' <- (gs ^. gameHandle . handleAction) pid act env
      if (not . isUnitAlive pid $ env') 
        then do
          (gs ^. gameHandle . handleDeath) env
          return $ packHasIOUI $ MainMenu (mainMenuUI chan)
      else return $ packHasIOUI (gs)
    keyPress :: Keys.Keys -> GameState -> IO (AnyHasUI IO CustomEvent)
    keyPress key gs
      | Just action <- keysToAction key = do
        let env = (gs ^. gameEnv)
        let pid = (gs ^. gamePlayerId)
        let chan = gs ^. gameBChan
        env' <- (gs ^. gameHandle . handleAction) pid action env
        if (not . isUnitAlive pid $ env')
          then do
            (gs ^. gameHandle . handleDeath) env
            return $ packHasIOUI $ MainMenu (mainMenuUI chan)
          else return $ packHasIOUI gs
    keyPress (Keys.Letter 'i') gs = 
      return . packHasIOUI $ Inventory {
        _inventoryEnv = gs ^. gameEnv ,
        _inventoryHandle = gs ^. gameHandle ,
        _inventoryPlayerId = gs ^. gamePlayerId,
        _inventoryBChan = gs ^. gameBChan
      }
    keyPress (Keys.Letter 'q') gs = do
      let chan = gs ^. gameBChan
      (gs ^. gameHandle . handleQuitGame) (gs ^. gameEnv)
      return $ packHasIOUI $ MainMenu (mainMenuUI chan)
    keyPress _ st = return $ packHasIOUI st

{-     customEvent (UpdateEnvUsingTransaction t) gs = do
      let env' = snd $ runGameEnv (Transaction.applyTransaction t) (gs ^. gameRealEnv)
      return $ packHasIOUI (gs {_gameRealEnv = env', _gameFakeEnv = env'})
    customEvent (UpdateEnvUsingMemento m) gs = do
      let env' = loadEnvironmentState m
      return $ packHasIOUI (gs {_gameRealEnv = env', _gameFakeEnv = env'}) -}
    customEvent (UpdateEnvironment env) gs =
      return $ packHasIOUI (gs {_gameEnv = env})

    renderMap = do
      playerPosition <- getUnitPosition pid env
      traceShowM playerPosition
      playerPortrait <- getUnitPortrait pid env
      visible <- Set.fromList . fmap positionXY <$> getVisibleToUnit pid env
      seenByPlayer <- getSeenByPlayer pid env
      traceShowM seenByPlayer
      seenOnLevel <- seenAtLevel (playerPosition ^. posLevel) <$> getSeenByPlayer pid env
      traceShowM seenOnLevel
      level <- getLevelByUnitId pid env
      let mobs = getActiveMobs env
      let mobPositions = rights $ map (flip getUnitPosition $ env) mobs
      let mobPortraits = rights $ map (flip getUnitPortrait $ env) mobs
      let players = getActivePlayers env
      let playerPositions = rights $ map (flip getUnitPosition $ env) players
      let playerPortraits = rights $ map (flip getUnitPortrait $ env) players
      return $ do
        GameUIDesc.setMapTerrain (renderCell  <$> level ^. lvlMap . cells)
        GameUIDesc.setMapHasBeenSeenByPlayer (`Set.member` seenOnLevel)
        GameUIDesc.setMapIsVisibleToPlayer (`Set.member` visible)
        GameUIDesc.setMapMobs $ zip (map positionXY mobPositions) mobPortraits
        GameUIDesc.setMapPlayers $ zip (map positionXY playerPositions) playerPortraits
        GameUIDesc.setMapMainPlayer (positionXY playerPosition) playerPortrait
    renderStats = do
      player <- getPlayerByPlayerId pid env
      let pStats = player ^. playerUnitData . stats
      let pLevellingStats = player ^. playerLevelling
      return $ 
        GameUIDesc.setStats
            [ ("Health", show (pStats ^. health)),
              ("Attack power", show (pStats ^. attackPower)),
              ("Shield", show (pStats ^. shield)),
              ("Level", show (pStats ^. level)),
              ("Experience", show (pLevellingStats ^. experience)),
              ("Skill points", show (pLevellingStats ^. skillPoints))
            ]


inventoryUI :: Environment -> PlayerId -> UI IO InventoryState CustomEvent
inventoryUI env pid = makeInventoryUI $
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
    InventoryUI.setCustomEventHandler customEvent
  where
    inv = fromRight emptyInventory $ getUnitInventory pid env
    defaultSlot = maybe "free"
    clickSlot i is = do
      env' <- (is ^. inventoryHandle . handleClickSlot) pid i env 
      return $ packHasIOUI (is {_inventoryEnv = env'})
    clickItem i is = do
      env' <- (is ^. inventoryHandle . handleClickItem) pid i env 
      return $ packHasIOUI (is {_inventoryEnv = env'})
    close is = 
      return $ packHasIOUI (Game {
        _gamePlayerId = is ^. inventoryPlayerId,
        _gameEnv = is ^. inventoryEnv,
        _gameHandle = is ^. inventoryHandle,
        _gameBChan = is ^. inventoryBChan
      })
    customEvent (UpdateEnvironment env) is = 
      return $ packHasIOUI (is {_inventoryEnv = env})

mainMenuUI :: BChan CustomEvent -> UI IO MainMenuState CustomEvent
mainMenuUI chan =
  makeListMenuUI $ do
    ListMenuDesc.setTitle "Main menu"
    ListMenuDesc.addItem "random" (const randomGame) -- TODO use random generator or at least ask user to input a seed
    ListMenuDesc.addItem "load last game" (const loadLastGame)
    ListMenuDesc.addItemPure "load level" (const $ packHasIOUI $ MainMenu (loadLvlMenuUI chan))
    ListMenuDesc.addItem "test level" (const testLevelGame)
    ListMenuDesc.addItem "create multiplayer game" undefined
    ListMenuDesc.addItem "join multiplayer game" undefined
    ListMenuDesc.addItemPure "quit" (const . packHasIOUI $ EndState)
    ListMenuDesc.selectItem 0
  where
    loadLastGame = do
      loaded <- loadGame "autosave"
      case loaded of
        Prelude.Left _ -> return $ packHasIOUI $ MainMenu (mainMenuUI chan)
        Prelude.Right env -> startSinglePlayerGame chan (loadEnvironmentState env)
    randomGame = do
      rnd <- getStdRandom random
      let env = randomEnvironment rnd
      startSinglePlayerGame chan env
    testLevelGame = startSinglePlayerGame chan testEnvironment


loadLevel :: String -> IO GameLevel
loadLevel name = do
  levelEither <- getLevelByName name
  let level1 = fromRight testGameLevel levelEither
  return level1

loadLvlMenuUI :: BChan CustomEvent -> UI IO MainMenuState CustomEvent
loadLvlMenuUI chan =
  makeListMenuUI $ do
    ListMenuDesc.setTitle "Load level"
    ListMenuDesc.addItem "level 1" (const $ loadLevelAndPack "Level_1")
    ListMenuDesc.addItem "level 2" (const $ loadLevelAndPack "Level_2")
    ListMenuDesc.addItem "level 3" (const $ loadLevelAndPack "Level_3")
    ListMenuDesc.addItemPure "back" (const $ packHasIOUI $ MainMenu (mainMenuUI chan))
    ListMenuDesc.selectItem 0
    where
      loadLevelAndPack s = do
        lvl <- loadLevel s
        let env = testEnvironmentWithLevel lvl
        startSinglePlayerGame chan env


singlePlayerHandleAction chan pid action env = do
  let trans = Transaction.unitAction pid action
  let env' = snd $ runGameEnv (Transaction.applyTransaction trans) env
  writeBChan chan (UpdateEnvironment env')
  return env

singlePlayerHandleClickSlot chan pid i env = do
  let trans = Transaction.clickSlot pid i
  let env' = snd $ runGameEnv (Transaction.applyTransaction trans) env
  writeBChan chan (UpdateEnvironment env')
  return env

singlePlayerHandleClickItem chan pid i env = do
  let trans = Transaction.clickItem pid i
  let env' = snd $ runGameEnv (Transaction.applyTransaction trans) env
  writeBChan chan (UpdateEnvironment env')
  return env

singlePlayerHandleDeath env = do
  removeGame "autosave"

singlePlayerHandleQuitGame env = do
  saveGame "autosave" $ getEnvState env 

startSinglePlayerGame :: BChan CustomEvent -> Environment -> IO (AnyHasUI IO CustomEvent)
startSinglePlayerGame chan env = do
  let g = Game {
    _gamePlayerId = pid,
    _gameEnv = env,
    _gameHandle = handle,
    _gameBChan = chan
  }
  return $ packHasIOUI g
  where
    pid = makePlayerId 1
    handle = Handle {
    _handleDeath = singlePlayerHandleDeath,
    _handleQuitGame = singlePlayerHandleQuitGame,
    _handleAction = singlePlayerHandleAction chan,
    _handleClickSlot = singlePlayerHandleClickSlot chan,
    _handleClickItem = singlePlayerHandleClickItem chan
    }

testEnvironmentWithLevel :: GameLevel -> Environment
testEnvironmentWithLevel level =
  makeEnvironment
    [ourPlayer]
    [ makeDefaultMob (makeUnitData 0 (3, 3) 'U') Aggressive ]
    [level]
  where
    ourPlayer = makeSomePlayer $ makeUnitData 0 (level ^. lvlMap . entrance) 'λ'

randomEnvironment :: Int -> Environment
randomEnvironment seed =
  makeEnvironment
    [ourPlayer]
    []
    [lvl]
  where
    lvl = fst $ randomBSPGeneratedLevel (GU.Space (GU.Coord 0 0) (GU.Coord 50 50)) (GeneratorParameters 10 1.7 5) $ mkStdGen seed
    startCoord = _entrance $ _lvlMap lvl
    ourPlayer = makeSomePlayer $ makeUnitData 0 startCoord 'λ'

testEnvironment :: Environment
testEnvironment =
  makeEnvironment
    [ourPlayer]
    [ makeDefaultMob (makeUnitData 0 (3, 3) 'U') Aggressive
    , makeDefaultMob (makeUnitData 0 (4, 6) 'U') (Passive (4, 6))
    , makeDefaultMob (makeUnitData 0 (5, 6) 'U') Avoiding
    ]
    [testGameLevel]
  where
    ourPlayer = makeSomePlayer $ makeUnitData 0 (7, 9) 'λ'

makeUnitData :: Int -> (Int, Int) -> Char -> UnitData
makeUnitData level position render =
  createUnitData
    (uncheckedPosition level position)
    (Stats.Stats 10 10 10 1)
    Game.Unit.TimedUnitOps.empty
    someInventory
    (createWeapon "drugged fist" (effectAtom (damage 1) >> effectTypical "confuse") 'A')
    render
  where
    someInventory =
        addItem (weaponToItem $ createWeapon "saber" (effectAtom (damage 5)) '?') $
        addItem (wearableToItem $ createWearable "pointy hat" Head (effectAtom (heal 10)) (return ()) '^') $
        addItem (wearableToItem $ createWearable "uncomfortable shoes" Legs (effectAtom confuse) (return ()) '"')
        emptyInventory

makeSomePlayer :: UnitData -> Player 
makeSomePlayer = makeDefaultPlayer . (stats . health %~ (*2))

