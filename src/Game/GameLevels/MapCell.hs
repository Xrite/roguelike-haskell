{-# LANGUAGE TemplateHaskell #-}

-- | Module responsible for working with a map cell

module Game.GameLevels.MapCell
  ( MapCell (..)
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

-- | Describes items that belongs to the cell
data MapCellState =
  MapCellState
    { _floorItems :: [Item] }
makeLenses ''MapCellState

-- | Map cell is described by its properties and its items
data MapCell =
  MapCell
    {
     -- | Defines map cell's properties
     _cellType :: MapCellType
     -- | Defines map cell's items
    , _cellState :: MapCellState
    }
makeLenses ''MapCell

makeCell :: MapCellType -> MapCellState -> MapCell
makeCell = MapCell

-- | Creates cell with no items with the given cellType
makeEmptyCell :: MapCellType -> MapCell
makeEmptyCell cellType' = makeCell cellType' $ MapCellState []

-- | Renders cell's first item if it presents 
-- otherwise render cell depending on its type
renderCell :: MapCell -> Char
renderCell cell = maybe (cell ^. cellType . cellRender) itemRender (headMay $ cell ^. cellState . floorItems)
