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
import qualified Data.Set as Set
import Game.Environment
import Game.FileIO.FileIO (getLevelByName)
import Game.GameLevels.GameLevel
import Game.GameLevels.GenerateLevel (randomBSPGeneratedLevel, testGameLevel)
import Game.GameLevels.Generation.BSPGen (GeneratorParameters (..))
import qualified Game.GameLevels.Generation.GenerationUtil as GU
import Game.Item (createWeapon)
import Game.Modifiers.EffectAtom
import Game.Modifiers.EffectDesc (effectAtom, effectTypical)
import Game.Modifiers.UnitOp
import Game.Modifiers.UnitOpFactory (makeUnitOpFactory, UnitOpFactory)
import Game.Unit.Action
import Game.Unit.Control
import Game.Unit.Inventory (emptyInventory)
import Game.Unit.Stats as Stats
import Game.Unit.TimedUnitOps (empty)
import Game.Unit.Unit
import System.Random (mkStdGen, getStdRandom, random)
import qualified UI.Descriptions.GameUIDesc as GameUIDesc
import qualified UI.Descriptions.ListMenuDesc as ListMenuDesc
import UI.Keys as Keys
import UI.UI

data GameState
  = Game Environment
  | EndState

newtype MainMenuState = MainMenu (UI IO MainMenuState)

makeLenses ''GameState

instance HasUI IO GameState where
  getUI (Game env) = gameUI env
  getUI EndState = terminalUI

instance HasUI IO MainMenuState where
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

gameUI :: (Applicative m, HasUI m GameState, HasUI m MainMenuState) => Environment -> UI m GameState
gameUI env = makeGameUIPure $
  do
    fst $ runGameEnv tryRender env
    fst $ runGameEnv tryGetStats env
    GameUIDesc.setArrowPress arrowPress
    GameUIDesc.setKeyPress keyPress
  where
    --    arrowPress :: (HasUI ma GameState) => Arrows -> GameState -> AnyHasUI ma
    arrowPress arrow (Game e) = packHasIOUI . Game . snd $ runGameEnv (makeTurn $ arrowsToAction arrow) e
    arrowPress _ st = packHasIOUI st
    --    keyPress :: Keys.Keys -> GameState -> AnyHasUI m
    keyPress key (Game e) | Just action <- keysToAction key = packHasIOUI . Game . snd $ runGameEnv (makeTurn action) e
    keyPress (Keys.Letter 'q') (Game _) = packHasIOUI $ MainMenu mainMenuUI
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
        return $ GameUIDesc.setStats
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
    [ makeMob (makeUnitData (3, 3) 'U') Aggressive ]
    [level]
    confusedFactory
  where
    ourPlayer = makeSomePlayer $ makeUnitData (level ^. lvlMap . entrance) 'λ'

randomEnvironment :: Int -> Environment
randomEnvironment seed =
  makeEnvironment
    ourPlayer
    []
    [lvl]
    confusedFactory
  where
    lvl = fst $ randomBSPGeneratedLevel (GU.Space (GU.Coord 0 0) (GU.Coord 50 50)) (GeneratorParameters 10 1.7 5) $ mkStdGen seed
    startCoord = _entrance $ _lvlMap lvl
    ourPlayer = makeSomePlayer $ makeUnitData startCoord 'λ'

testEnvironment :: Environment
testEnvironment =
  makeEnvironment
    ourPlayer
    [ makeMob (makeUnitData (3, 3) 'U') Aggressive,
      makeMob (makeUnitData (4, 6) 'U') (Passive (4, 6)),
      makeMob (makeUnitData (5, 6) 'U') Avoiding
    ]
    [testGameLevel]
    confusedFactory
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
    (createWeapon "drugged fist" (effectAtom (damage 1) >> effectTypical "confuse") 'A')
    render

makeSomePlayer :: UnitData -> Player
makeSomePlayer = makePlayer . (stats . health %~ (*2))

makeTurn :: Action -> GameEnv ()
makeTurn playerAction = do
  player <- getPlayer
  _ <- runExceptT (evalAction player playerAction)
  mobs <- getActiveMobs
  _ <- runExceptT $ traverse (\u -> getAction u >>= evalAction u) mobs
  units <- getActiveUnits
  _ <- runExceptT $ traverse (`affectUnit` tickTimedEffects) units
  return ()

confusedEffect :: UnitOp ()
confusedEffect = setTimedUnitOp 10 (const $ effectAtom confuse)

confusedFactory :: UnitOpFactory
confusedFactory = makeUnitOpFactory $ Map.singleton "confuse" confusedEffect