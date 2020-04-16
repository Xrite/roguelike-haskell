module Game.Controller.MovesSpec where

import qualified Game.Controller.Moves as SUT
import Game.Environment (makeEnvironment, Environment, UnitId, makeUnitId, unitById, getCurrentLevel)
import Game.GameLevels.GenerateLevel (testGameLevel)
import Game.Item (createWeapon)
import Game.Unit.Inventory (emptyInventory)
import Game.Unit.Mob
import Game.Unit.Player (makePlayer)
import Game.Unit.Stats (Stats(..))
import Game.Unit.Unit (_position)
import Game.Unit.TimedModifiers (empty)
import Game.GameLevels.GameLevel (_lvlMap, getMapSize)
import Game.Unit.Unit (createUnitData, packUnit, UnitData, asUnitData)
import Test.Hspec
import Data.Maybe (isNothing, isJust)
import Game.Unit.Action
import Control.Lens ((^.))

spec :: Spec
spec = do
  canMoveSpec
  moveUnitSpec
  attackCoordSafeSpec
  makeActionSpec

canMoveSpec :: Spec
canMoveSpec =
  describe "canMove tests" $ do
    it "can't move in a wall" $ canMoveTo (3, 6) `shouldBe` False
    it "can't move into other units" $ canMoveTo (7, 8) `shouldBe` False
    it "can move to free place" $ canMoveTo (3, 7) `shouldBe` True
    it "can move to \"from\" corner" $ canMoveTo (xFrom, yFrom) `shouldBe` True
    it "can move to the \"to\" corner" $ canMoveTo (xTo, yTo) `shouldBe` True
    it "can't move outside of the map (min x - 1)" $ canMoveTo (xFrom - 1, 10) `shouldBe` False
    it "can't move outside of the map (max x + 1)" $ canMoveTo (xTo + 1, 10) `shouldBe` False
    it "can't move outside of the map (min y - 1)" $ canMoveTo (10, yFrom - 1) `shouldBe` False
    it "can't move outside of the map (max y + 1)" $ canMoveTo (10, yTo + 1) `shouldBe` False
  where
    canMoveTo position = SUT.canMove (packUnit ourPlayer) position testEnvironment
    ((xFrom, yFrom), (xTo, yTo)) = getMapSize $ _lvlMap $ getCurrentLevel testEnvironment

moveUnitSpec :: Spec
moveUnitSpec =
  describe "moveUnit tests" $ do
    it "can't move in a wall" $ moveTo (3, 6) `shouldSatisfy` isNothing
    it "can't move in others" $ moveTo (14, 15) `shouldSatisfy` isNothing
    it "can move in yourself" $ let pos = (7, 8) in moveTo pos `shouldSatisfy` maybe False ((== pos) . _position . asUnitData . unitById (makeUnitId 0))
    it "can move in free places" $ let pos = (3, 7) in moveTo pos `shouldSatisfy` maybe False ((== pos) . _position . asUnitData . unitById (makeUnitId 0))
  where
    moveTo position = SUT.maybeMoveUnit (makeUnitId 0) position testEnvironment

attackCoordSafeSpec :: Spec
attackCoordSafeSpec =
  describe "attackCoordSafe tests" $ do
    it "can attack others" $ attackAt (14, 15) `shouldSatisfy` isJust
    it "can't attack youself" $ attackAt (7, 8) `shouldSatisfy` isNothing
    it "can't attack nothing" $ attackAt (4, 4) `shouldSatisfy` isNothing
   where
    attackAt position = SUT.maybeAttackCoordSafe (makeUnitId 0) position testEnvironment

makeActionSpec :: Spec
makeActionSpec =
  describe "makeActionTest" $ do
    it "can't walk in a wall" $ takeAction 2 (Move Negative Zero) `shouldSatisfy` isNothing
    it "can walk on ground" $ takeAction 2 (Move Negative Negative) `shouldSatisfy` isJust
    it "can stay" $ takeAction 2 (Move Zero Zero) `shouldSatisfy` isJust
    it "can attack" $ takeAction 2 (Move Positive Zero) `shouldSatisfy` isJust
  where takeAction uidNumber action = SUT.maybeMakeAction (makeUnitId uidNumber) action testEnvironment

ourPlayer = makePlayer $ makeUnitData (5, 6)

testEnvironment :: Environment
testEnvironment =
  makeEnvironment
    ourPlayer
    [ packUnit $ Mob $ makeUnitData (7, 8)
    , packUnit $ Mob $ makeUnitData (14, 15)
    , packUnit $ Mob $ makeUnitData (4, 6)
    , packUnit $ Mob $ makeUnitData (5, 6)
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
    'U'
    undefined
