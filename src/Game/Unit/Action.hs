module Game.Unit.Action
  ( Action(..)
  , Direction(..)
  , directionToInt
  , changeCoord
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
