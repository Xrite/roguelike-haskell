{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Game where

import Brick.BChan
import Text.Read (readMaybe)
import Control.Lens
import Control.Monad.Except
import Data.Either (fromRight, rights)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Debug.Trace
import Game.Environment
import Game.EnvironmentGeneration
import Game.FileIO.FileIO (getLevelByName)
import Game.FileIO.SaveGame
import Game.GameControl
import Game.GameLevels.GameLevel
import Game.GameLevels.GenerateLevel (randomBSPGeneratedLevel, testGameLevel)
import Game.GameLevels.Generation.BSPGen (GeneratorParameters (..))
import qualified Game.GameLevels.Generation.GenerationUtil as GU
import Game.GameLevels.MapCell (renderCell)
import Game.Item
import Game.Modifiers.EffectAtom
import Game.Modifiers.EffectDesc (effectAtom, effectTypical)
import Game.Modifiers.UnitOp
import Game.Modifiers.UnitOpFactory (UnitOpFactory, makeUnitOpFactory)
import Game.Multiplayer.Client as Client
import Game.Multiplayer.Server (SessionId)
import Game.Position
import Game.Transaction (Transaction)
import qualified Game.Transaction as Transaction
import Game.Unit.Action
import Game.Unit.Control
import Game.Unit.Inventory
import Game.Unit.Stats as Stats
import Game.Unit.TimedUnitOps (empty)
import Game.Unit.Unit
import Network.Socket (HostName, PortNumber)
import System.Random (getStdRandom, mkStdGen, random)
import qualified UI.Descriptions.GameUIDesc as GameUIDesc
import qualified UI.Descriptions.InventoryUIDesc as InventoryUI
import qualified UI.Descriptions.ListMenuDesc as ListMenuDesc
import qualified UI.Descriptions.EnterDataUIDesc as EnterDataUI
import qualified UI.Keys as Keys
import UI.UI
import Control.Concurrent (threadDelay, forkIO)

data CustomEvent
  = --  = UpdateEnvUsingTransaction Transaction
    --  | UpdateEnvUsingMemento EnvMemento
    UpdateEnvironment Environment

data Handle
  = Handle
      { _handleDeath :: Environment -> IO (),
        _handleQuitGame :: Environment -> IO (),
        _handleAction :: PlayerId -> Action -> Environment -> IO Environment,
        _handleClickSlot :: PlayerId -> Int -> Environment -> IO Environment,
        _handleClickItem :: PlayerId -> Int -> Environment -> IO Environment
      }

data GameState
  = Game
      { _gamePlayerId :: PlayerId,
        _gameEnv :: Environment,
        _gameHandle :: Handle,
        _gameBChan :: BChan CustomEvent
      }

data InventoryState
  = Inventory
      { _inventoryPlayerId :: PlayerId,
        _inventoryEnv :: Environment,
        _inventoryHandle :: Handle,
        _inventoryBChan :: BChan CustomEvent
      }

data EndState = EndState

newtype MainMenuState = MainMenu (UI IO MainMenuState CustomEvent)

newtype EnterDataState = EnterData (UI IO EnterDataState CustomEvent)

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

instance HasUI IO EnterDataState CustomEvent where
  getUI (EnterData ui) = ui

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
      return . packHasIOUI $
        Inventory
          { _inventoryEnv = gs ^. gameEnv,
            _inventoryHandle = gs ^. gameHandle,
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
      --traceShowM playerPosition
      playerPortrait <- getUnitPortrait pid env
      --traceShowM playerPortrait
      visible <- Set.fromList . fmap positionXY <$> getVisibleToUnit pid env
      seenByPlayer <- getSeenByPlayer pid env
      --traceShowM seenByPlayer
      seenOnLevel <- seenAtLevel (playerPosition ^. posLevel) <$> getSeenByPlayer pid env
      --traceShowM seenOnLevel
      level <- getLevelByUnitId pid env
      let mobs = getActiveMobs env
      let mobPositions = rights $ map (flip getUnitPosition $ env) mobs
      let mobPortraits = rights $ map (flip getUnitPortrait $ env) mobs
      let players = getActivePlayers env
      let playerPositions = rights $ map (flip getUnitPosition $ env) players
      let playerPortraits = rights $ map (flip getUnitPortrait $ env) players
      return $ do
        GameUIDesc.setMapTerrain (renderCell <$> level ^. lvlMap . cells)
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
      return $
        packHasIOUI
          ( Game
              { _gamePlayerId = is ^. inventoryPlayerId,
                _gameEnv = is ^. inventoryEnv,
                _gameHandle = is ^. inventoryHandle,
                _gameBChan = is ^. inventoryBChan
              }
          )
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
    ListMenuDesc.addItem "join multiplayer game" (const . return . packHasIOUI . EnterData $ joinServerUI chan)
    ListMenuDesc.addItemPure "quit" (const . packHasIOUI $ EndState)
    ListMenuDesc.selectItem 0
  where
    loadLastGame = do
      loaded <- loadGame "autosave"
      case loaded of
        Prelude.Left _ -> return $ packHasIOUI $ MainMenu (mainMenuUI chan)
        Prelude.Right envMemento -> let env = loadEnvironmentState envMemento in startSinglePlayerGame chan (playerId env) env
    randomGame = do
      rnd <- getStdRandom random
      let (pid, env) = randomEnvironment rnd
      startSinglePlayerGame chan pid env
    testLevelGame = let (pid, env) = testEnvironment in startSinglePlayerGame chan pid env
    playerId env = getActivePlayers env !! 0

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
      let (pid, env) = testEnvironmentWithLevel lvl
      startSinglePlayerGame chan pid env

joinServerUI :: BChan CustomEvent -> UI IO EnterDataState CustomEvent
joinServerUI chan = 
  makeEnterDataUI $ do
    EnterDataUI.setTitle "Join server. Use enter to join and delete to delete last symbol"
    EnterDataUI.addAcceptInputHandler acceptInput
    EnterDataUI.addCloseHandler (const $ return $ packHasIOUI $ MainMenu (mainMenuUI chan))
  where
    parseInput input = do
      let ip = takeWhile (not . (== ':')) $ input 
      port <- readMaybe . drop 1 . dropWhile (not . (== ':')) $ input
      return (ip, port)
    acceptInput input _ = do
      case parseInput input of
        Nothing -> return $ mainMenu
        Just (ip, port) -> do
          mClientHandle <- Client.setupClient ip port
          case mClientHandle of
            Nothing -> return mainMenu
            Just clientHandle -> do
              sessions <- Client.getSessions clientHandle
              let newUI = MainMenu (selectSessionMenuUI chan clientHandle sessions)
              return $ packHasIOUI newUI

    mainMenu = packHasIOUI $ MainMenu (mainMenuUI chan)

      


selectSessionMenuUI :: BChan CustomEvent -> ClientHandle -> [SessionId] -> UI IO MainMenuState CustomEvent
selectSessionMenuUI chan clientHandle ss =
  makeListMenuUI $ do
    ListMenuDesc.setTitle "Select session"
    mapM (\sid -> ListMenuDesc.addItem (show sid) (const $ joinSession sid)) ss
    ListMenuDesc.addItem "new session" (const $ startNewSession)
    ListMenuDesc.addItemPure "back" (const $ packHasIOUI $ MainMenu (mainMenuUI chan))
    ListMenuDesc.selectItem 0
  where
    joinSession sid = do
      mPid <- Client.addNewPlayerToSession clientHandle sid
      case mPid of
        Nothing -> return $ packHasIOUI $ MainMenu (mainMenuUI chan)
        Just pid -> tryGetState sid pid
    tryGetState sid pid = do
      mEnv <- Client.getSessionState clientHandle sid
      case mEnv of
        Nothing -> return $ packHasIOUI $ MainMenu (mainMenuUI chan)
        Just envM -> do
          let env = loadEnvironmentState envM
          startMultiPlayerGame chan clientHandle sid pid env
    startNewSession = do
      mSid <- Client.createNewSession clientHandle
      case mSid of
        Nothing -> return $ packHasIOUI $ MainMenu (mainMenuUI chan)
        Just sid -> joinSession sid


singlePlayerHandleAction chan pid action env = do
  let env' = snd $ runGameEnv (makeTurn pid action) env
  writeBChan chan (UpdateEnvironment env')
  return env

singlePlayerHandleClickSlot chan pid i env = do
  let env' = snd $ runGameEnv (doClickSlot pid i) env
  writeBChan chan (UpdateEnvironment env')
  return env

singlePlayerHandleClickItem chan pid i env = do
  let env' = snd $ runGameEnv (doClickItem pid i) env
  writeBChan chan (UpdateEnvironment env')
  return env

singlePlayerHandleDeath env = do
  removeGame "autosave"

singlePlayerHandleQuitGame env = do
  saveGame "autosave" $ getEnvState env

startSinglePlayerGame :: BChan CustomEvent -> PlayerId -> Environment -> IO (AnyHasUI IO CustomEvent)
startSinglePlayerGame chan pid env = do
  let g =
        Game
          { _gamePlayerId = pid,
            _gameEnv = env,
            _gameHandle = handle,
            _gameBChan = chan
          }
  return $ packHasIOUI g
  where
    handle =
      Handle
        { _handleDeath = singlePlayerHandleDeath,
          _handleQuitGame = singlePlayerHandleQuitGame,
          _handleAction = singlePlayerHandleAction chan,
          _handleClickSlot = singlePlayerHandleClickSlot chan,
          _handleClickItem = singlePlayerHandleClickItem chan
        }

multiPlayerHandleAction clientHandle sid chan pid action env = do
  --let env' = snd $ runGameEnv (makeTurn pid action) env
  --writeBChan chan (UpdateEnvironment env')
  forkIO $ Client.makeAction clientHandle sid pid action
  return env

multiPlayerHandleClickSlot clientHandle sid chan pid i env = do
  --let env' = snd $ runGameEnv (doClickSlot pid i) env
  --writeBChan chan (UpdateEnvironment env')
  forkIO $ Client.clickSlot clientHandle sid pid i
  return env

multiPlayerHandleClickItem clientHandle sid chan pid i env = do
  --let env' = snd $ runGameEnv (doClickItem pid i) env
  --writeBChan chan (UpdateEnvironment env')
  forkIO $ Client.clickItem clientHandle sid pid i
  return env

multiPlayerHandleDeath clientHandle sid pid env = do
  Client.removePlayerFromSession clientHandle sid pid

--removeGame "autosave"

multiPlayerHandleQuitGame clientHandle sid pid env = do
  Client.removePlayerFromSession clientHandle sid pid

--saveGame "autosave" $ getEnvState env

multiPlayerStateUpdate clientHandle sid chan = do
  mEnv <- Client.getSessionState clientHandle sid
  case mEnv of
    Nothing -> return ()
    Just envM -> do
      let env = loadEnvironmentState envM
      writeBChan chan (UpdateEnvironment env)
  threadDelay $ 200000



startMultiPlayerGame :: BChan CustomEvent -> ClientHandle -> SessionId -> PlayerId -> Environment -> IO (AnyHasUI IO CustomEvent)
startMultiPlayerGame chan clientHandle sid pid env = do
  forkIO $ forever $ multiPlayerStateUpdate clientHandle sid chan
  let g =
        Game
          { _gamePlayerId = pid,
            _gameEnv = env,
            _gameHandle = handle,
            _gameBChan = chan
          }
  return $ packHasIOUI g
  where
    handle =
      Handle
        { _handleDeath = multiPlayerHandleDeath clientHandle sid pid,
          _handleQuitGame = multiPlayerHandleQuitGame clientHandle sid pid,
          _handleAction = multiPlayerHandleAction clientHandle sid chan,
          _handleClickSlot = multiPlayerHandleClickSlot clientHandle sid chan,
          _handleClickItem = multiPlayerHandleClickItem clientHandle sid chan
        }
