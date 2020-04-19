{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Module responsible for working with the level's map
module Game.GameLevels.GameLevel
  ( getCell,
    getMapSize,
    inBounds,
    makeGameLevel,
    lvlMap,
    makeMap,
    maybeGetCellAt,
    isEntranceAt,
    isExitAt,
    GameLevel (..),
    Map (..),
  )
where

import Control.Lens ((^.), makeLenses)
import Data.Array.IArray
import Game.GameLevels.MapCell

data Map
  = Map
      { _entrance :: (Int, Int),
        _exit :: (Int, Int),
        _cells :: Array (Int, Int) MapCell
      }

instance Show Map where
  show (Map entrance exit cells) = "Map { _entrance = " ++ show entrance ++ ", _exit = " ++ show exit ++ ", _cells =\n" ++ c ++ "\n}"
    where
      ((xFrom, yFrom), (xTo, yTo)) = bounds cells
      c = foldl1 (\a b -> a ++ "\n" ++ b) [[renderCell (cells ! (i, j)) | i <- [xFrom..xTo]] | j <- [yFrom..yTo]]

makeLenses ''Map

data GameLevel
  = GameLevel
      { _lvlMap :: Map
      }
  deriving (Show)

makeLenses ''GameLevel

getCell :: (Int, Int) -> Map -> MapCell
getCell i mp = (mp ^. cells) ! i

-- | Return cell at position or nothing if position is out of bounds
maybeGetCellAt :: (Int, Int) -> GameLevel -> Maybe MapCell
maybeGetCellAt i g = if inBounds m i then Just (getCell i m) else Nothing
  where
    m = g ^. lvlMap

isEntranceAt :: (Int, Int) -> GameLevel -> Bool
isEntranceAt i g = g ^. lvlMap . entrance == i

isExitAt :: (Int, Int) -> GameLevel -> Bool
isExitAt i g = g ^. lvlMap . exit == i

getMapSize :: Map -> ((Int, Int), (Int, Int))
getMapSize mp = bounds (mp ^. cells)

inBounds :: Map -> (Int, Int) -> Bool
inBounds mp (x, y) = inBounds1D xFrom xTo x && inBounds1D yFrom yTo y
  where
    ((xFrom, yFrom), (xTo, yTo)) = getMapSize mp
    inBounds1D from to coord = from <= coord && coord <= to

makeGameLevel :: Map -> GameLevel
makeGameLevel = GameLevel

makeMap :: (Int, Int) -> (Int, Int) -> Array (Int, Int) MapCell -> Map
makeMap = Map
