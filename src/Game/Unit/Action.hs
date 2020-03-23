module Game.Unit.Action
  ( Action(..)
  , Direction(..)
  , directionToInt
  , changeCoord
  , moveUp
  , moveDown
  , moveRight
  , moveLeft
  ) where

import Data.Bifunctor (bimap)

data Direction = Positive
               | Negative
               | Zero

data Action = Move Direction Direction

directionToInt :: Direction -> Int
directionToInt Positive = 1
directionToInt Negative = -1
directionToInt Zero = 0

changeCoord :: Direction -> Direction -> (Int, Int) -> (Int, Int)
changeCoord xDir yDir = bimap (+ directionToInt xDir) (+ directionToInt yDir)

moveUp :: Action
moveUp = Move Zero Negative

moveDown :: Action
moveDown = Move Zero Positive

moveRight :: Action
moveRight = Move Positive Zero

moveLeft :: Action
moveLeft = Move Negative Zero