module Game.ActionEvaluation where

import Game.Environment
import Game.Modifiers.Modifier as Modifier
import Game.Unit.Action as Action

evalAction :: UnitId -> Action -> GameEnv ()
evalAction u (Move xDir yDir) = do
    position <- affectUnit u Modifier.getPosition
    let newPosition = Action.changeCoord xDir yDir position
    unitAtPos <- unitByCoord newPosition
    case unitAtPos of
        Nothing -> moveUnit u newPosition >> return ()
        Just enemy -> envAttack u enemy