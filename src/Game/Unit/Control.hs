module Game.Unit.Control where

import Control.Lens
import Control.Monad.Except
import Data.Graph.Inductive
import Data.List
import {-# SOURCE #-} Game.Environment
import Game.GameLevels.GameLevel
import Game.GameLevels.MapCell
import Game.GameLevels.MapCellType
import Game.GameLevels.PathFinding
import Game.GameLevels.Visibility
import qualified Game.Modifiers.UnitOp as UnitOp
import Game.Unit.Action
import {-# SOURCE #-} Game.Unit.Unit

data TaggedControl = Aggressive | Passive | Avoiding | DoNothing

getControl :: TaggedControl -> UnitId -> FailableGameEnv UnitIdError Action
getControl Aggressive = aggressiveControl
getControl Passive = passiveControl
getControl Avoiding = avoidingControl
getControl DoNothing = const $ return stayAtPosition

aggressiveControl :: UnitId -> FailableGameEnv UnitIdError Action
aggressiveControl uid = do
  lvl <- lift getCurrentLevel
  playerPos <- affectUnit (playerId undefined) UnitOp.getPosition
  unitPos <- affectUnit uid UnitOp.getPosition
  maybeStats <- affectUnit uid UnitOp.getStats
  allUnits <- lift getActiveUnits >>= traverse (`affectUnit` UnitOp.getPosition)
  let passability cell = case maybeStats of
        Nothing -> False
        Just stats -> (cell ^. cellType . passable) stats
  let visibility cell = case maybeStats of
        Nothing -> False
        Just stats -> (cell ^. cellType . transparent) stats
  let path = findPath passability (lvl ^. lvlMap) unitPos playerPos (allUnits \\ [playerPos, unitPos])
  if canSee (lvl ^. lvlMap) visibility unitPos playerPos
    then case path of
      Nothing -> return stayAtPosition
      Just (_ : (x, y) : _) -> return $ deltaToAction (x - fst unitPos, y - snd unitPos)
      Just _ -> return stayAtPosition
    else return stayAtPosition

passiveControl :: UnitId -> FailableGameEnv UnitIdError Action
passiveControl mob = do
  dx <- lift $ randomRGameEnv (-1, 1)
  dy <- lift $ randomRGameEnv (-1, 1)
  return $ deltaToAction (dx, dy)

avoidingControl :: UnitId -> FailableGameEnv UnitIdError Action
avoidingControl mob = return $ stayAtPosition 
