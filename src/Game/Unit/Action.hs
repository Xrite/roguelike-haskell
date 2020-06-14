module Game.Unit.Action
  ( Action (..),
    Direction (..),
    directionToInt,
    intToDirection,
    deltaToAction,
    actionToDelta,
    changeCoord,
    moveUp,
    moveDown,
    moveRight,
    moveLeft,
    stayAtPosition,
  )
where

import Data.Bifunctor (bimap)

data Direction
  = Positive
  | Negative
  | Zero
  deriving (Eq)

data Action = Move Direction Direction deriving (Eq)

directionToInt :: Direction -> Int
directionToInt Positive = 1
directionToInt Negative = -1
directionToInt Zero = 0

intToDirection :: Int -> Direction
intToDirection i
  | i > 0 = Positive
  | i == 0 = Zero
  | i < 0 = Negative

deltaToAction :: (Int, Int) -> Action
deltaToAction (dx, dy) = Move (intToDirection dx) (intToDirection dy)

actionToDelta :: Action -> (Int, Int)
actionToDelta (Move moveX moveY) = (directionToInt moveX, directionToInt moveY)

changeCoord :: Direction -> Direction -> (Int, Int) -> (Int, Int)
changeCoord xDir yDir = bimap (+ directionToInt xDir) (+ directionToInt yDir)

moveUp :: Action
moveUp = Move Zero Positive

moveDown :: Action
moveDown = Move Zero Negative

moveRight :: Action
moveRight = Move Positive Zero

moveLeft :: Action
moveLeft = Move Negative Zero

stayAtPosition :: Action
stayAtPosition = Move Zero Zero
