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
import Game.GameControl (GameCfg(..), makeAction, clickSlot, clickItem, interpret)
import Game.GameLevels.MapCell (renderCell)

data CustomEvent 
  = UpdateEnvUsingTransaction Transaction
  | UpdateEnvUsingMemento EnvMemento

data Handle =
  Handle {
    handleDeath :: Environment -> IO (),
    handleQuitGame :: Environment -> IO (),
    handleStartGame :: IO Environment
  }

data GameState 
  = Game {
    gameRealEnv :: Environment,
    gameFakeEnv :: Environment,
    gameGameCfg :: GameCfg,
    gameHandle :: Handle
  }

data InventoryState 
  = Inventory {
    inventoryRealEnv :: Environment,
    inventoryFakeEnv :: Environment,
    inventoryGameCfg :: GameCfg,
    inventoryHandle :: Handle
  }

data EndState = EndState

newtype MainMenuState = MainMenu (UI IO MainMenuState CustomEvent)

makeLenses ''GameState

instance HasUI IO GameState CustomEvent where
  getUI gs = gameUI (gameFakeEnv gs) (playerId $ gameGameCfg gs)

instance HasUI IO InventoryState CustomEvent where
  getUI is = inventoryUI (inventoryFakeEnv is) (playerId $ inventoryGameCfg is)

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
    fromRight (return ()) renderMap
    fromRight (return ()) renderStats
    GameUIDesc.setArrowPress arrowPress
    GameUIDesc.setKeyPress keyPress
    GameUIDesc.setCustomEventHandler customEvent
  where
    --    arrowPress :: (HasUI ma GameState) => Arrows -> GameState -> AnyHasUI ma
    arrowPress arrow gs = do
      let cmd = makeAction $ arrowsToAction arrow
      env' <- interpret (gameGameCfg gs) env
      if (not . isPlayerAlive $ newEnv) 
        then do
          handleDeath $ gameHandle gs
          return $ packHasIOUI $ MainMenu mainMenuUI 
      else return $ packHasIOUI (gs {gameFakeEnv = env'})
    arrowPress _ st = return $ packHasIOUI st
    --    keyPress :: Keys.Keys -> GameState -> AnyHasUI m
    keyPress key gs 
      | Just action <- keysToAction key = do
        let cmd = makeAction action
        env' <- interpret (gameGameCfg gs) env
        if (not . isPlayerAlive $ newEnv) 
          then do
            handleDeath $ gameHandle gs
            return $ packHasIOUI $ MainMenu mainMenuUI 
        else return $ packHasIOUI (gs {gameFakeEnv = env'})
    keyPress (Keys.Letter 'i') gs = 
      return . packHasIOUI $ Inventory e
    keyPress (Keys.Letter 'q') gs = do
      saveGame "autosave" $ getEnvState e
      return $ packHasIOUI $ MainMenu mainMenuUI
    keyPress _ st = return $ packHasIOUI st

    customEvent (UpdateEnvUsingTransaction t) gs = do
      let env' = runGameEnv (applyTransaction t) (gameRealEnv gs)
      return $ packHasIOUI (gs {gameRealEnv = env', gameFakeEnv = env'})
    customEvent (UpdateEnvUsingMemento m) gs = do
      let env' = loadEnvironmentState m
      return $ packHasIOUI (gs {gameRealEnv = env', gameFakeEnv = env'})

    renderMap = do
      playerPosition <- getUnitPosition pid env
      playerPortrait <- getUnitPortrait pid env
      visible <- Set.fromList . fmap positionXY <$> getVisibleToUnit pid env
      seenOnLevel <- seenAtLevel (playerPosition ^. posLevel) <$> getSeenByPlayer pid env
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

mainMenuUI :: UI IO MainMenuState CustomEvent
mainMenuUI =
  makeListMenuUI $ do
    ListMenuDesc.setTitle "Main menu"
    ListMenuDesc.addItem "random" (const (packHasIOUI . Game . randomEnvironment <$> getStdRandom random)) -- TODO use random generator or at least ask user to input a seed
    ListMenuDesc.addItem "load last game" (const
         (do loaded <- loadGame "autosave"
             return $ case loaded of
               Prelude.Left _ -> packHasIOUI $ MainMenu mainMenuUI
               Prelude.Right env -> packHasIOUI $ Game $ loadEnvironmentState env)
         )
    ListMenuDesc.addItemPure "load level" (const $ packHasIOUI $ MainMenu loadLvlMenuUI)
    ListMenuDesc.addItemPure "test level" (const (packHasIOUI $ Game testEnvironment))
    ListMenuDesc.addItem "create multiplayer game" undefined
    ListMenuDesc.addItem "join multiplayer game" undefined
    ListMenuDesc.addItemPure "quit" (const . packHasIOUI $ EndState)
    ListMenuDesc.selectItem 0

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
  where
    (inv, _) = runGameEnv getPlayerInventory env
    defaultSlot = maybe "free"
    clickSlot i is = do
      let cmd = Transaction.clickSlot i
      env' <- interpret (inventoryGameCfg is) env cmd
      return $ packHasIOUI (is {gameFakeEnv = env'})
    clickItem i is = do
      let cmd = Transaction.clickItem i
      env' <- interpret (inventoryGameCfg is) env cmd
      return $ packHasIOUI (is {gameFakeEnv = env'})
    close is = do
      return $ packHasIOUI (Game {
        gameRealEnv = inventoryRealEnv is,
        gameFakeEnv = inventoryFakeEnv is,
        gameGameCfg = inventoryGameCfg is,
        gameHandle = inventoryHandle is
      })

loadLevel :: String -> IO GameLevel
loadLevel name = do
  levelEither <- getLevelByName name
  let level1 = fromRight testGameLevel levelEither
  return level1

loadLvlMenuUI :: UI IO MainMenuState CustomEvent
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
    [ makeMob (makeUnitData (3, 3) 'U') Aggressive ]
    [level]
  where
    ourPlayer = makeSomePlayer $ makeUnitData (level ^. lvlMap . entrance) 'λ'

randomEnvironment :: Int -> Environment
randomEnvironment seed =
  makeEnvironment
    ourPlayer
    []
    [lvl]
  where
    lvl = fst $ randomBSPGeneratedLevel (GU.Space (GU.Coord 0 0) (GU.Coord 50 50)) (GeneratorParameters 10 1.7 5) $ mkStdGen seed
    startCoord = _entrance $ _lvlMap lvl
    ourPlayer = makeSomePlayer $ makeUnitData startCoord 'λ'

testEnvironment :: Environment
testEnvironment =
  makeEnvironment
    ourPlayer
    [ makeMob (makeUnitData (3, 3) 'U') Aggressive
    , makeMob (makeUnitData (4, 6) 'U') (Passive (4, 6))
    , makeMob (makeUnitData (5, 6) 'U') Avoiding
    ]
    [testGameLevel]
  where
    ourPlayer = makeSomePlayer $ makeUnitData (7, 9) 'λ'

makeUnitData :: (Int, Int) -> Char -> UnitData Position
makeUnitData position render =
  createUnitData
    position
    0
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

makeSomePlayer :: UnitData Position -> Player Position
makeSomePlayer = makePlayer . (stats . health %~ (*2))

