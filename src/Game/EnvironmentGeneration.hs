module Game.EnvironmentGeneration where

import Brick.BChan
import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Either (fromRight, rights)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Debug.Trace
import Game.Environment
import Game.FileIO.FileIO (getLevelByName)
import Game.FileIO.SaveGame
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
import Game.Position
import Game.Transaction (Transaction)
import qualified Game.Transaction as Transaction
import Game.Unit.Action
import Game.Unit.Control
import Game.Unit.Inventory
import Game.Unit.Stats
import Game.Unit.TimedUnitOps (empty)
import Game.Unit.Unit
import System.Random (getStdRandom, mkStdGen, random)
import qualified UI.Descriptions.GameUIDesc as GameUIDesc
import qualified UI.Descriptions.InventoryUIDesc as InventoryUI
import qualified UI.Descriptions.ListMenuDesc as ListMenuDesc
import qualified UI.Keys as Keys
import UI.UI

testEnvironmentWithLevel :: GameLevel -> (PlayerId, Environment)
testEnvironmentWithLevel level = runGameEnv addUnits env
  where
    env = makeEnvironment [] [] [level]
    addUnits = do
      pid <- addDefaultPlayer
      addDefaultAggressiveMob
      return pid

randomEnvironment :: Int -> (PlayerId, Environment)
randomEnvironment seed = runGameEnv addUnits env
  where
    env = randomEnvironmentWithoutUnits seed
    addUnits = do
      pid <- addDefaultPlayer
      addDefaultPassiveMob
      addDefaultAggressiveMob
      addDefaultAvoidingMob
      return pid

addDefaultPlayer :: GameEnv PlayerId
addDefaultPlayer = do
  env <- get
  let fps = getFreePositionsOnLevel env playerStats 0
  idx <- randomRGameEnv (0, length fps - 1)
  let fp = fps !! idx
  let player = defaultPlayerWithStats fp playerStats
  pid <- addPlayerToEnvironment player
  addUnitToQueue pid
  return pid
  where
    playerStats = defaultStats

addDefaultPassiveMob :: GameEnv MobId
addDefaultPassiveMob = do
  env <- get
  let fps = getFreePositionsOnLevel env mobStats 0
  idx <- randomRGameEnv (0, length fps - 1)
  let fp = fps !! idx
  let mob = defaultPassiveMobWithStats fp mobStats
  mid <- addMobToEnvironment mob
  addUnitToQueue mid
  return mid
  where
    mobStats = defaultStats

addDefaultAggressiveMob :: GameEnv MobId
addDefaultAggressiveMob = do
  env <- get
  let fps = getFreePositionsOnLevel env mobStats 0
  idx <- randomRGameEnv (0, length fps - 1)
  let fp = fps !! idx
  let mob = defaultAggressiveMobWithStats fp mobStats
  mid <- addMobToEnvironment mob
  addUnitToQueue mid
  return mid
  where
    mobStats = defaultStats

addDefaultAvoidingMob :: GameEnv MobId
addDefaultAvoidingMob = do
  env <- get
  let fps = getFreePositionsOnLevel env mobStats 0
  idx <- randomRGameEnv (0, length fps - 1)
  let fp = fps !! idx
  let mob = defaultAvoidingMobWithStats fp mobStats
  mid <- addMobToEnvironment mob
  addUnitToQueue mid
  return mid
  where
    mobStats = defaultStats

randomEnvironmentWithoutUnits :: Int -> Environment
randomEnvironmentWithoutUnits seed =
  makeEnvironment
    []
    []
    [lvl]
  where
    lvl =
      fst
        $ randomBSPGeneratedLevel
          (GU.Space (GU.Coord 0 0) (GU.Coord 50 50))
          (GeneratorParameters 10 1.7 5)
        $ mkStdGen seed

testEnvironment :: (PlayerId, Environment)
testEnvironment = runGameEnv addUnits env
  where
    ourPlayer = makeSomePlayer $ makeUnitData 0 (7, 9) 'λ'
    env =
      makeEnvironment
        []
        []
        [testGameLevel]
    addUnits = do
      pid <- addPlayerToEnvironment ourPlayer
      addUnitToQueue pid
      addMobToEnvironment (makeDefaultMob (makeUnitData 0 (3, 3) 'A') Aggressive) >>= addUnitToQueue
      addMobToEnvironment (makeDefaultMob (makeUnitData 0 (4, 6) 'P') (Passive (4, 6))) >>= addUnitToQueue
      addMobToEnvironment (makeDefaultMob (makeUnitData 0 (5, 6) 'C') Avoiding) >>= addUnitToQueue
      return pid

makeUnitData :: Int -> (Int, Int) -> Char -> UnitData
makeUnitData level position render =
  createUnitData
    (uncheckedPosition level position)
    defaultStats
    Game.Unit.TimedUnitOps.empty
    someInventory
    (createWeapon "drugged fist" (effectAtom (damage 1) >> effectTypical "confuse") 'A')
    render
  where
    someInventory =
      addItem (weaponToItem $ createWeapon "saber" (effectAtom (damage 5)) '?')
        $ addItem (wearableToItem $ createWearable "pointy hat" Head (effectAtom (heal 10)) (return ()) '^')
        $ addItem
          (wearableToItem $ createWearable "uncomfortable shoes" Legs (effectAtom confuse) (return ()) '"')
          emptyInventory

makeSomePlayer :: UnitData -> Player
makeSomePlayer = makeDefaultPlayer . (stats . health %~ (* 2))

sampleInventory :: Inventory
sampleInventory =
  addItem (weaponToItem $ createWeapon "saber" (effectAtom (damage 5)) '?')
    $ addItem (wearableToItem $ createWearable "pointy hat" Head (effectAtom (heal 10)) (return ()) '^')
    $ addItem
      (wearableToItem $ createWearable "uncomfortable shoes" Legs (effectAtom confuse) (return ()) '"')
      emptyInventory

weaponDruggedFist :: WeaponItem
weaponDruggedFist = (createWeapon "drugged fist" (effectAtom (damage 1) >> effectTypical "confuse") 'A')

defaultPlayerWithStats :: Position -> Stats -> Player
defaultPlayerWithStats pos st =
  makeSomePlayer $
    createUnitData
      pos
      st
      Game.Unit.TimedUnitOps.empty
      sampleInventory
      weaponDruggedFist
      'λ'

defaultPassiveMobWithStats :: Position -> Stats -> Mob
defaultPassiveMobWithStats pos st =
  makeDefaultMob unitData (Passive $ positionXY pos)
  where
    unitData =
      createUnitData
        pos
        st
        Game.Unit.TimedUnitOps.empty
        sampleInventory
        weaponDruggedFist
        'P'

defaultAggressiveMobWithStats :: Position -> Stats -> Mob
defaultAggressiveMobWithStats pos st =
  makeDefaultMob unitData Aggressive
  where
    unitData =
      createUnitData
        pos
        st
        Game.Unit.TimedUnitOps.empty
        sampleInventory
        weaponDruggedFist
        'A'

defaultAvoidingMobWithStats :: Position -> Stats -> Mob
defaultAvoidingMobWithStats pos st =
  makeDefaultMob unitData Avoiding
  where
    unitData =
      createUnitData
        pos
        st
        Game.Unit.TimedUnitOps.empty
        sampleInventory
        weaponDruggedFist
        'X'
