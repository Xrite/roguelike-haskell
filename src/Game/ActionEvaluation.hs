module Game.ActionEvaluation where

import Control.Monad (void, when)
import Control.Monad.Except
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import {-# SOURCE #-} Game.Environment
import Game.Modifiers.UnitOp as UnitOp
import Game.Unit.Action as Action
import Safe (atMay)

type ActionEvaluator = UnitId -> Action -> FailableGameEnv UnitIdError ()

basicEvaluation :: ActionEvaluator
basicEvaluation u (Move xDir yDir) = do
  position <- affectUnit u UnitOp.getPosition
  let newPosition = Action.changeCoord xDir yDir position
  unitAtPos <- lift $ unitByCoord newPosition
  case unitAtPos of
    Nothing -> void $ moveUnit u newPosition
    Just other -> when (other /= u) $ envAttack u other

defaultEvaluation :: ActionEvaluator
defaultEvaluation = confuseAwareDecorator basicEvaluation

confuseAwareDecorator :: ActionEvaluator -> ActionEvaluator
confuseAwareDecorator eval u dir = do
  isConfused <- affectUnit u UnitOp.getConfusion
  let eval' =
        if isConfused
          then confusedDecorator eval
          else eval
  eval' u dir

-- | Changes ActionEvaluator so that performed action is changed randomly.
--  Move direction is changed to an adjacent with probability 0.5.
confusedDecorator :: ActionEvaluator -> ActionEvaluator
confusedDecorator eval u dir = do
  rand <- lift $ randomRGameEnv (0.0, 1.0)
  let dir'
        | rand > changeDirectionProbability = dir
        | rand > (changeDirectionProbability / 2) = nextDirection dir
        | otherwise = prevDirection dir
  eval u dir'
  where
    changeDirectionProbability = 0.25 :: Double
    directionsCycle =
      [ Move Positive Positive,
        Move Positive Zero,
        Move Positive Negative,
        Move Zero Negative,
        Move Negative Negative,
        Move Negative Zero,
        Move Negative Positive,
        Move Zero Positive,
        Move Positive Positive
      ]
    nextDirection (Move Zero Zero) = Move Zero Zero
    nextDirection d = head $ drops (== d) directionsCycle
    prevDirection = foldl1 (.) $ replicate 7 nextDirection
    drops f (x : xs) = if f x then xs else drops f xs
    drops _ [] = error "element no found"
