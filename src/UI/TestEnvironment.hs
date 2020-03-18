module UI.TestEnvironment (testEnvironment) where

import Game.Environment (makeEnvironment, Environment, UnitId, makeUnitId, unitById)
import Game.GameLevels.GenerateLevel (testGameLevel)
import Game.Item (createWeapon)
import Game.Unit.Inventory (emptyInventory)
import Game.Unit.Mob
import Game.Unit.Player (makePlayer, Player)
import Game.Unit.Stats (Stats(..))
import Game.Unit.Unit
import Game.Unit.TimedEffects (empty)
import Game.Unit.Unit (createUnitData, packUnit, UnitData, asUnitData)
import Data.Maybe (isNothing, isJust)
import Game.Unit.Action

ourPlayer = makeSomePlayer $ makeUnitData (5, 6)

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
    0

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

makeSomePlayer :: UnitData -> Player
makeSomePlayer unitData = makePlayer unitData