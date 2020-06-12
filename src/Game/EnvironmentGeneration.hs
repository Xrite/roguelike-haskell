module Game.EnvironmentGeneration where
    
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
import Game.Unit.Stats
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
  
randomEnvironmentWithoutUnits :: Int -> Environment
randomEnvironmentWithoutUnits seed = 
  makeEnvironment
  []
  []
  [lvl]
  where
    lvl = fst $ randomBSPGeneratedLevel
      (GU.Space (GU.Coord 0 0) (GU.Coord 50 50)) (GeneratorParameters 10 1.7 5) $ mkStdGen seed

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
  makeSomePlayer $ createUnitData
    pos
    defaultStats
    Game.Unit.TimedUnitOps.empty
    sampleInventory
    weaponDruggedFist
    'λ'
    

