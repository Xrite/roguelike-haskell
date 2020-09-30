module Game.GameLevels.GameLevelSerialization () where

import Data.Binary
import Game.GameLevels.MapCell (MapCellState, MapCell)
import Game.ItemSerialization ()
import Game.GameLevels.GameLevel (GameLevel, Map)

instance Binary MapCellState
instance Binary MapCell

instance Binary Map
instance Binary GameLevel 