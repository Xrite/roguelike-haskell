module Game.Levels.GenerateLevel
  ( testLevel
  ) where

import Game.Levels.MapCellTypeImpl
import Game.Levels.MapCellType
import Game.Levels.Level
import Game.Levels.MapCell
import Data.Array.ST
import Control.Monad.ST

testLevel :: Level
testLevel =
  makeLevel $
  makeMap $
  runSTArray $ do
    arrType <- newArray ((0, 0), (20, 20)) ground :: ST s (STArray s (Int, Int) MapCellType)
    writeArray arrType (3, 6) wall
    mapArray makeEmptyCell arrType
