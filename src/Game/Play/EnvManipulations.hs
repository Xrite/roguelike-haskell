{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
module Game.Play.EnvManipulations where

import Game.Environment
import Game.Unit.Unit
import Control.Lens ((^.), (%~))
import Game.GameLevels.GameLevel
import Data.Foldable (find)
import Game.GameLevels.MapCell
import Game.GameLevels.MapCellType
import Data.Maybe (isNothing)
import Game.Effect
import Util

-- | Moves a unit to a place if they can go there. Returns 'Nothing' if it is occupied by someone else or if it is a wall.
moveUnit :: Environment -> Int -> (Int, Int) -> Maybe Environment
moveUnit env unitNumber coord =
  if cantMoveThere
    then Nothing
    else Just $ units %~ setAt unitNumber (anyUnitApplyEffect (setCoord coord) movingUnit) $ env
  where
    movingUnit = _units env !! unitNumber
    cantMoveThere = canGo env movingUnit (_position $ anyUnitAsData movingUnit) coord

findByCoord :: Environment -> (Int, Int) -> Maybe AnyUnit
findByCoord env coord = find ((== coord) . _position . anyUnitAsData) $ _units env

canGo :: Environment -> AnyUnit -> (Int, Int) -> (Int, Int) -> Bool
canGo env unit oldCoord newCoord = oldCoord == newCoord || canMoveThere
  where
      occupyingUnitMaybe = findByCoord env newCoord
      placeOccupied = isNothing occupyingUnitMaybe
      cell = getCell newCoord $ getCurrentLevel env ^. lvlMap
      canMoveThere = not placeOccupied && (cell ^. cellType . passable $ anyUnitAsData unit ^. stats)