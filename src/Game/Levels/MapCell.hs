{-# LANGUAGE TemplateHaskell, Rank2Types #-}

module Game.Levels.MapCell
  ( MapCell
  , makeCell
  , makeEmptyCell
  , cellType
  , floorItems
  , renderCell
  ) where

import Control.Lens (makeLenses, (^.))
import Game.Item
import Game.Levels.MapCellType
import Safe (headMay)

data MapCell =
  MapCell
    { _cellType :: MapCellType
    , _floorItems :: [Item]
    }

makeLenses ''MapCell

makeCell :: MapCellType -> [Item] -> MapCell
makeCell = MapCell

makeEmptyCell :: MapCellType -> MapCell
makeEmptyCell cellType' = makeCell cellType' []

renderCell :: MapCell -> Char
renderCell cell = maybe (cell ^. cellType . cellRender) itemRender (headMay $ cell ^. floorItems)
