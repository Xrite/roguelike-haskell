module Game.Controller.MovesSpec where

import qualified Game.Controller.Moves as SUT
import Game.Environment (makeEnvironment, Environment, UnitId, makeUnitId)
import Game.GameLevels.GenerateLevel (testGameLevel)
import Game.Item (createWeapon)
import Game.Unit.Inventory (emptyInventory)
import Game.Unit.Mob
import Game.Unit.Player (makePlayer)
import Game.Unit.Stats (Stats(..))
import Game.Unit.TimedEffects (empty)
import Game.Unit.Unit (createUnitData, packUnit, UnitData)
import Test.Hspec
import Data.Maybe (isNothing, isJust)

spec :: Spec
spec = do
  canMoveSpec
  moveUnitSpec

canMoveSpec :: Spec
canMoveSpec = do
  describe "Can't move canMove tests" $ do
    it "can't move in a wall" $ canMoveTo (3, 6) `shouldBe` False
    it "can't move into other units" $ canMoveTo (7, 8) `shouldBe` False
  describe "Can move canMove tests" $
    it "can move to free place" $ canMoveTo (3, 7) `shouldBe` True
 where
  canMoveTo position = SUT.canMove (packUnit ourPlayer) position testEnvironment

moveUnitSpec :: Spec
moveUnitSpec =
  describe "moveUnit tests" $ do
    it "can't move in wall" $ moveTo (3, 6) `shouldSatisfy` isNothing
    it "can't move in others" $ moveTo (14, 15) `shouldSatisfy` isNothing
    it "can move in yourself" $ moveTo (7, 8) `shouldSatisfy` isJust
    it "can move in free places" $ moveTo (3, 7) `shouldSatisfy` isJust
  where
    moveTo position = SUT.maybeMoveUnit (makeUnitId 0) position testEnvironment


ourPlayer = makePlayer $ makeUnitData (5, 6)

testEnvironment :: Environment
testEnvironment =
  makeEnvironment
    ourPlayer
    [ packUnit $ Mob $ makeUnitData (7, 8)
    , packUnit $ Mob $ makeUnitData (14, 15)
    ]
    [testGameLevel]

makeUnitData :: (Int, Int) -> UnitData
makeUnitData position =
  createUnitData
    position
    0
    (Stats 10 10 10 1)
    empty
    emptyInventory
    (createWeapon "weapon" (return ()) 'A')
    undefined
