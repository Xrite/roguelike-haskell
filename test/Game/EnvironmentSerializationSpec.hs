module Game.EnvironmentSerializationSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Game.Environment
import Game.Unit.Unit
import Game.Unit.Stats
import qualified Game.Unit.TimedUnitOps as TimedUnitOps
import Game.EnvironmentSerialization
import Game.Unit.Control
import Game.Item
import Game.Unit.Inventory
import Game.Modifiers.EffectDesc
import Game.Modifiers.EffectAtom
import Control.Lens ((%~))
import Game.GameLevels.GenerateLevel (testGameLevel)
import System.Random (mkStdGen)
import Data.Binary (Binary, decode, encode)
import Game.GameLevels.GameLevel (GameLevel)
import Data.Functor.Classes (Show1)
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap

throughCode :: Binary a => a -> a
throughCode = decode . encode

testCoding :: (Show a, Eq a, Binary a) => a -> Expectation
testCoding a = throughCode a `shouldBe` a

instance Show GameLevel where
  show _ = "GameLevel"
instance Show Player where
  show _ = "Player"
instance Show Mob where
  show _ = "Mob"
instance Show AnyUnit where
  show _ = "AnyUnit"
instance Show1 EffectDescDSL where

instance Show EnvMemento where
  show _ = "Memento"

spec :: Spec
spec =
  describe "Serialization" $ do
    it "stdGen" $ testCoding $ mkStdGen 42
    it "GameLevel" $ testCoding testGameLevel
    it "EffectDesc (simple)" $ testCoding $ effectTypical "confuse"
    it "EffectDesc" $ testCoding $ effectAtom (damage 1) >> effectTypical "confuse"
    it "Player" $ testCoding $ makeSomePlayer $ makeUnitData (7, 9) 'λ'
    it "Mob" $ testCoding $ makeMob (makeUnitData (3, 3) 'U') Aggressive
    it "Set of coords" $ testCoding (Set.empty :: Set.Set (Int, Int))
    it "IntMap of coords" $ testCoding (IntMap.empty :: IntMap.IntMap (Int, Int))
    it "Environment state (simple)" $ testCoding $ getEnvState simpleEnvironment
    it "Environment state" $ testCoding $ getEnvState testEnvironment

simpleEnvironment :: Environment
simpleEnvironment =
  makeEnvironment
    ourPlayer
    []
    []
  where
    ourPlayer = makeSomePlayer $ createUnitData
      (0, 0)
      0
      (Stats 10 10 10 1)
      TimedUnitOps.empty
      emptyInventory
      (createWeapon "fist" (return ()) 'A')
      '@'

testEnvironment :: Environment
testEnvironment =
  makeEnvironment
    somePlayer
    [ makeMob (makeUnitData (3, 3) 'U') Aggressive
    , makeMob (makeUnitData (4, 6) 'U') (Passive (4, 6))
    , makeMob (makeUnitData (5, 6) 'U') Avoiding
    ]
    [testGameLevel]

makeUnitData :: (Int, Int) -> Char -> UnitData
makeUnitData position =
  createUnitData
    position
    0
    (Stats 10 10 10 1)
    TimedUnitOps.empty
    emptyInventory
    (createWeapon "drugged fist" (effectAtom (damage 1) >> effectTypical "confuse") 'A')

makeSomePlayer :: UnitData -> Player
makeSomePlayer = makePlayer . (stats . health %~ (*2))

somePlayer :: Player
somePlayer = makeSomePlayer $ makeUnitData (7, 9) 'λ'