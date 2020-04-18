module Game.ActionEvaluation where

import {-# SOURCE #-} Game.Environment
import Game.Modifiers.UnitOp as UnitOp
import Game.Unit.Action as Action

defaultEvaluation :: UnitId -> Action -> GameEnv ()
defaultEvaluation u (Move xDir yDir) = do
    position <- affectUnit u UnitOp.getPosition
    let newPosition = Action.changeCoord xDir yDir position
    unitAtPos <- unitByCoord newPosition
    case unitAtPos of
        Nothing -> moveUnit u newPosition >> return ()
        Just enemy -> envAttack u enemy