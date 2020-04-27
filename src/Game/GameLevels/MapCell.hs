{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Module responsible for working with a map cell

module Game.GameLevels.MapCell
  ( MapCell (..)
  , makeCell
  , makeEmptyCell
  , cellTypeKey
  , cellType
  , floorItems
  , renderCell
  ) where

import Control.Lens (makeLenses, (^.))
import Game.Item
import Game.GameLevels.MapCellType
import Game.GameLevels.MapCellTypeImpl
import Safe (headMay)
import GHC.Generics (Generic)
import Control.Lens.Getter (Getter)

-- | Describes items that belong to the cell
data MapCellState =
  MapCellState
    { _floorItems :: [Item] }
  deriving (Generic)
makeLenses ''MapCellState

-- | Map cell is described by its properties and its items
data MapCell =
  MapCell
    {
     -- | Defines map cell's properties
     _cellTypeKey :: MapCellTypeKey
     -- | Defines map cell's items
    , _cellState :: MapCellState
    }
  deriving (Generic)

makeLenses ''MapCell

-- | Flyweight for MapCellType constructed by cell
cellType :: Getter MapCell MapCellType
cellType f cell = cell <$ f (cellTypeByKey $ _cellTypeKey cell)

makeCell :: MapCellTypeKey -> MapCellState -> MapCell
makeCell = MapCell

-- | Creates cell with no items with the given cellType
makeEmptyCell :: MapCellTypeKey -> MapCell
makeEmptyCell cellType' = makeCell cellType' $ MapCellState []

-- | Renders cell's first item if it presents 
-- otherwise render cell depending on its type
renderCell :: MapCell -> Char
renderCell cell = maybe (cell ^. cellType . cellRender) itemRender (headMay $ cell ^. cellState . floorItems)
