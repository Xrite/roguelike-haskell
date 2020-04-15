
-- | This module provides some functions for possible moves of units.
module Game.Controller.Moves where

import Control.Lens ((%~), (^.))
import Control.Monad (guard)
import Data.Maybe (isNothing)
import Game.Modifiers.Modifier
import Game.Environment
import Game.GameLevels.GameLevel
import Game.GameLevels.MapCell
import Game.GameLevels.MapCellType
import Game.Unit.Action
import Game.Unit.Unit
  ( AnyUnit
  , _position
  , applyModifier
  , asUnitData
  , position
  , stats
  , Action(..)
  )
import Control.Applicative ((<|>))

-- | Simulates unit taking an action. Returns 'Nothing' if the action is impossible
maybeMakeAction :: UnitId -> Action -> Environment -> Maybe Environment
maybeMakeAction unitId (Move xDir yDir) env = maybeMakeMoveUnbound unitId newCoord env
  where
    currentCoord = asUnitData (unitById unitId env) ^. position
    newCoord = changeCoord xDir yDir currentCoord

-- | Allows unit to make a move considering distance.
maybeMakeMove :: UnitId -> (Int, Int) -> Environment -> Maybe Environment
maybeMakeMove unitId coord env = do
  let movingUnit = unitById unitId env
  guard $ close (asUnitData movingUnit ^. position) coord
  maybeMakeMoveUnbound unitId coord env
  where
    close (x1, y1) (x2, y2) = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1

-- | What happens if a unit wants to make a move to a location. Does not consider distance.
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
  guard $ asUnitData (unitById attackerId env) ^. position /= coord
  occupyingUnitId <- unitIdByCoord coord env
  return $ envAttack attackerId occupyingUnitId env

-- | Moves a unit to a place if the place is free. Returns 'Nothing' if it is occupied by someone else or if it is a wall.
maybeMoveUnit :: UnitId -> (Int, Int) -> Environment -> Maybe Environment
maybeMoveUnit unitId coord env =
  if canMoveThere
    then Just $ unitLensById unitId %~ applyModifier (setCoord coord) $ env
    else Nothing
  where
    movingUnit = unitById unitId env
    canMoveThere = canMove movingUnit coord env

-- | Checks if a unit could be moved to a location
-- Position is allowed if it is either equals to the previous position or the place is passable and there are 
-- no one else there.
canMove :: AnyUnit -> (Int, Int) -> Environment -> Bool
canMove unit newCoord env = (oldCoord == newCoord) || (placeInBounds && placeFree {- would crash if not for laziness -})
  where
    mp = getCurrentLevel env ^. lvlMap
    placeInBounds = inBounds mp newCoord
    oldCoord = _position $ asUnitData unit
    occupyingUnitMaybe = unitByCoord newCoord env
    placeNotOccupied = isNothing occupyingUnitMaybe
    cell = getCell newCoord mp
    placeFree = placeNotOccupied && (cell ^. cellType . passable $ asUnitData unit ^. stats)

