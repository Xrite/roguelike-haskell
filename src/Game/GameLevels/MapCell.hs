{-# LANGUAGE TemplateHaskell #-}

module Game.GameLevels.MapCell
  ( MapCell
  , makeCell
  , makeEmptyCell
  , cellType
  , floorItems
  , renderCell
  ) where

import Control.Lens (makeLenses, (^.))
import Game.Item
import Game.GameLevels.MapCellType
import Safe (headMay)

data MapCellState =
  MapCellState
    { _floorItems :: [Item] }
makeLenses ''MapCellState

data MapCell =
  MapCell
    { _cellType :: MapCellType
    , _cellState :: MapCellState
    }
makeLenses ''MapCell

makeCell :: MapCellType -> MapCellState -> MapCell
makeCell = MapCell

makeEmptyCell :: MapCellType -> MapCell
makeEmptyCell cellType' = makeCell cellType' $ MapCellState []

renderCell :: MapCell -> Char
renderCell cell = maybe (cell ^. cellType . cellRender) itemRender (headMay $ cell ^. cellState . floorItems)

