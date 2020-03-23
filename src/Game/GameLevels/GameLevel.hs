{-# LANGUAGE TemplateHaskell, Rank2Types #-}

-- | Module responsible for working with the level's map

module Game.GameLevels.GameLevel
  ( getCell
  , getMapSize
  , makeGameLevel
  , lvlMap
  , makeMap
  , GameLevel (..)
  , Map (..)
  ) where

import Game.GameLevels.MapCell
import Control.Lens (makeLenses, (^.))
import Data.Array.IArray

newtype Map = Map{_cells :: Array (Int, Int) MapCell}
makeLenses ''Map

data GameLevel = GameLevel
  { _lvlMap :: Map
  }
makeLenses ''GameLevel

getCell :: (Int, Int) -> Map -> MapCell
getCell i mp = (mp ^. cells) ! i

getMapSize :: Map -> ((Int, Int), (Int, Int))
getMapSize mp = bounds (mp ^. cells)

makeGameLevel :: Map -> GameLevel
makeGameLevel = GameLevel

makeMap :: Array (Int, Int) MapCell -> Map
makeMap = Map
