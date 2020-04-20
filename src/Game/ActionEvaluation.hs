module Game.ActionEvaluation where

import Control.Monad (void)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import {-# SOURCE #-} Game.Environment
import Game.Modifiers.UnitOp as UnitOp
import Game.Unit.Action as Action
import Safe (atMay)

type ActionEvaluator = UnitId -> Action -> GameEnv ()

basicEvaluation :: ActionEvaluator
basicEvaluation u (Move xDir yDir) = do
  position <- affectUnit u UnitOp.getPosition
  let newPosition = Action.changeCoord xDir yDir position
  unitAtPos <- unitByCoord newPosition
  case unitAtPos of
    Nothing -> void $ moveUnit u newPosition
    Just enemy -> envAttack u enemy

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

-- |Changes ActionEvaluator so that performed action is changed randomly.
-- Move direction is changed to a adjacent with probability 0.5.
confusedDecorator :: ActionEvaluator -> ActionEvaluator
confusedDecorator eval u dir = do
  rand <- randomRGameEnv (0.0, 1.0)
  let dir' =
        if rand > changeDirectionProbability
          then dir
          else if rand > (changeDirectionProbability / 2)
                 then nextDirection dir
                 else prevDirection dir
  eval u dir'
  where
    changeDirectionProbability = 0.5 :: Double
    directionsCycle =
      [ Move Positive Positive
      , Move Positive Zero
      , Move Positive Negative
      , Move Zero Negative
      , Move Negative Negative
      , Move Negative Zero
      , Move Negative Positive
      , Move Zero Positive
      , Move Positive Positive
      ]
    nextDirection (Move Zero Zero) = Move Zero Zero
    nextDirection dir =
      fromMaybe
        (Move Zero Zero)
        (do idx <- fst <$> find ((== dir) . snd) (zip [1 ..] directionsCycle)
            atMay directionsCycle $ idx + 1)
    prevDirection = foldl1 (.) $ replicate 8 nextDirection
