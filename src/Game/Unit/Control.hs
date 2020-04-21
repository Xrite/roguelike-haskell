module Game.Unit.Control where

import Control.Lens
import Data.Graph.Inductive
import Data.List
import {-# SOURCE #-} Game.Environment
import Game.GameLevels.GameLevel
import Game.GameLevels.MapCell
import Game.GameLevels.MapCellType
import Game.GameLevels.PathFinding
import qualified Game.Modifiers.UnitOp as UnitOp
import Game.Unit.Action
import {-# SOURCE #-} Game.Unit.Unit
import Control.Monad.Except

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
  let f cell = case maybeStats of
        Nothing -> False
        Just stats -> (cell ^. cellType . passable) stats
  let path = findPath f (lvl ^. lvlMap) unitPos playerPos []
  case path of
    Nothing -> return stayAtPosition
    Just (_ : (x, y) : _) -> return $ deltaToAction (x - fst unitPos, y - snd unitPos)
    Just _ -> return stayAtPosition

passiveControl :: UnitId -> FailableGameEnv UnitIdError Action
passiveControl mob = return $ moveDown

avoidingControl :: UnitId -> FailableGameEnv UnitIdError Action
avoidingControl mob = return $ moveDown
