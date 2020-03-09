{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Game.Levels.Level
  ( getCell
  , makeLevel
  , lvlMap
  , makeMap
  , Level
  ) where

import Game.Levels.MapCell
import Control.Lens (makeLenses, (^.))
import Data.Array.IArray

newtype Map = Map{_cells :: Array (Int, Int) MapCell}
makeLenses ''Map

data Level = Level
  { _lvlMap :: Map
  }
makeLenses ''Level

getCell :: (Int, Int) -> Map -> MapCell
getCell i mp = (mp ^. cells) ! i

makeLevel :: Map -> Level
makeLevel = Level

makeMap :: Array (Int, Int) MapCell -> Map
makeMap = Map

-- ST versions
--getCellST :: (Int, Int) -> MapST s -> ST s MapCell
--getCellST i map = readArray (map ^. cellsST) i

--data MapST s = MapST
--  { _cellsST :: STArray s (Int, Int) MapCell }
--makeLenses ''MapST
