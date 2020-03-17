module Game.Controller.Controller where

import Control.Lens ((%~), (^.))
import Control.Monad.State
import Data.Foldable (find)
import Data.Maybe (isNothing)
import Game.Effect
import Game.Environment
import Game.GameLevels.GameLevel
import Game.GameLevels.MapCell
import Game.GameLevels.MapCellType
import Game.Unit.Mob (Mob)
import Game.Unit.Player (Player)
import Game.Unit.Unit (AnyUnit, _position, applyEffect, asUnitData, stats)
import PreludeUtil

-- | Moves a unit to a place if they can go there. Returns 'Nothing' if it is occupied by someone else or if it is a wall.
moveUnit :: Int -> (Int, Int) -> Environment -> Maybe Environment
moveUnit unitNumber coord env =
  if cantMoveThere
    then Nothing
    else Just $ units %~
         setAt unitNumber (applyEffect (setCoord coord) movingUnit) $
         env
  where
    movingUnit = _units env !! unitNumber
    cantMoveThere =
      canGo movingUnit (_position $ asUnitData movingUnit) coord env

findByCoord :: (Int, Int) -> Environment -> Maybe AnyUnit
findByCoord coord env = find ((== coord) . _position . asUnitData) $ _units env

canGo :: AnyUnit -> (Int, Int) -> (Int, Int) -> Environment -> Bool
canGo unit oldCoord newCoord env = oldCoord == newCoord || placeFree
  where
    occupyingUnitMaybe = findByCoord newCoord env
    placeOccupied = isNothing occupyingUnitMaybe
    cell = getCell newCoord $ getCurrentLevel env ^. lvlMap
    placeFree =
      not placeOccupied &&
      (cell ^. cellType . passable $ asUnitData unit ^. stats)
