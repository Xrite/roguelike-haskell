module Game.GameLevels.GenerateLevel
  ( testGameLevel
  ) where

import Game.GameLevels.MapCellTypeImpl
import Game.GameLevels.MapCellType
import Game.GameLevels.GameLevel
import Game.GameLevels.MapCell
import Data.Array.ST
import Control.Monad.ST

testGameLevel :: GameLevel
testGameLevel =
  makeGameLevel $
  makeMap $
  runSTArray $ do
    arrType <- newArray ((0, 0), (20, 20)) ground :: ST s (STArray s (Int, Int) MapCellType)
    writeArray arrType (3, 6) wall
    writeArray arrType (10, 2) ladderDown
    writeArray arrType (10, 17) ladderUp
    foldMap (\ i -> writeArray arrType i bush) [(x, 10) | x <- [2..15]]
    mapArray makeEmptyCell arrType
