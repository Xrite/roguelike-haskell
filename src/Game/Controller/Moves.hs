module Game.Controller.Moves where

import Control.Lens ((%~), (^.))
import Control.Monad (guard)
import Data.Maybe (isNothing)
import Game.Effect
import Game.Environment
import Game.GameLevels.GameLevel
import Game.GameLevels.MapCell
import Game.GameLevels.MapCellType
import Game.Unit.Unit
  ( AnyUnit
  , _position
  , applyEffect
  , asUnitData
  , position
  , stats
  )
import Control.Applicative ((<|>))

maybeMakeMove :: UnitId -> (Int, Int) -> Environment -> Maybe Environment
maybeMakeMove unitId coord env = do
  let movingUnit = unitById unitId env
  guard $ close (asUnitData movingUnit ^. position) coord
  maybeMakeMoveUnbound unitId coord env
  where
    close (x1, y1) (x2, y2) = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1

-- | What happens if a unit wants to make a move to a location. Does not consider distance
maybeMakeMoveUnbound :: UnitId -> (Int, Int) -> Environment -> Maybe Environment
maybeMakeMoveUnbound unitId coord env = maybeAttackCoordSafe unitId coord env <|> maybeMoveUnit unitId coord env

-- | Makes a unit attack a unit by provided coord if there is a unit there. Allows attacking yourself.
maybeAttackCoord :: UnitId -> (Int, Int) -> Environment -> Maybe Environment
maybeAttackCoord attackerId coord env =
  fmap ($ env) $ envAttack attackerId <$> occupyingUnitIdMaybe
  where
    occupyingUnitIdMaybe = unitIdByCoord coord env

-- | Makes a unit attack a unit by provided coord if there is a unit there. Prohibits attacking yourself.
maybeAttackCoordSafe :: UnitId -> (Int, Int) -> Environment -> Maybe Environment
maybeAttackCoordSafe attackerId coord env = do
  occupyingUnitId <- unitIdByCoord coord env
  guard $ asUnitData (unitById occupyingUnitId env) ^. position /= coord
  return $ envAttack attackerId occupyingUnitId env

-- | Moves a unit to a place if the place is free. Returns 'Nothing' if it is occupied by someone else or if it is a wall.
maybeMoveUnit :: UnitId -> (Int, Int) -> Environment -> Maybe Environment
maybeMoveUnit unitId coord env =
  if cantMoveThere
    then Nothing
    else Just $ unitLensById unitId %~ applyEffect (setCoord coord) $ env
  where
    movingUnit = unitById unitId env
    cantMoveThere =
      canMove movingUnit (_position $ asUnitData movingUnit) coord env

canMove :: AnyUnit -> (Int, Int) -> (Int, Int) -> Environment -> Bool
canMove unit oldCoord newCoord env = oldCoord == newCoord || placeFree
  where
    occupyingUnitMaybe = unitByCoord newCoord env
    placeOccupied = isNothing occupyingUnitMaybe
    cell = getCell newCoord $ getCurrentLevel env ^. lvlMap
    placeFree =
      not placeOccupied &&
      (cell ^. cellType . passable $ asUnitData unit ^. stats)
