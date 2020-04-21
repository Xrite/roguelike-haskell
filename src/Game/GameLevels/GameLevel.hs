{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Module responsible for working with the level's map
module Game.GameLevels.GameLevel
  ( getCell,
    getMapSize,
    inBounds,
    makeGameLevel,
    lvlMap,
    entrance,
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

makeLenses ''Map

data GameLevel
  = GameLevel
      { _lvlMap :: Map
      }

makeLenses ''GameLevel

-- | Unsafe get cell at position
getCell :: (Int, Int) -> Map -> MapCell
getCell i mp = (mp ^. cells) ! i

-- | Return cell at position or nothing if position is out of bounds
maybeGetCellAt :: (Int, Int) -> GameLevel -> Maybe MapCell
maybeGetCellAt i g = if inBounds m i then Just (getCell i m) else Nothing
  where
    m = g ^. lvlMap

-- | Check whether a cell at position is an entrance to the next level
isEntranceAt :: (Int, Int) -> GameLevel -> Bool
isEntranceAt i g = g ^. lvlMap . entrance == i

-- | Check whether a cell at position is an exit to the previous level
isExitAt :: (Int, Int) -> GameLevel -> Bool
isExitAt i g = g ^. lvlMap . exit == i

-- | Get map bounds
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