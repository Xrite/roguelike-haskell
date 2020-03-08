{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Game.Environment where

import Control.Lens (makeLenses)
import Data.Array.IArray
--import Data.Array.ST
--import Control.Monad.ST (runST)
--import GHC.IOArray

data Item = Item { _name :: String }

data Inventory = Inventory { _items :: [Item] }

data Stats = Stats { _health :: Int, _level :: Int }

data Player =
  Player { _position :: (Int, Int), _stats :: Stats, _inventory :: Inventory }

data MapCellType = MapCellType -- should be Unit instead of Player here
  { _render :: Char
  , _passable :: Player -> Bool -- cell can let only certain units pass
  , _transparent :: Player -> Bool
  }
makeLenses ''MapCellType

data MapCell = MapCell
  { _cellType :: MapCellType
  , _drop :: [Item]
  }
makeLenses ''MapCell

newtype Map = Map{_cells :: Array (Int, Int) MapCell}
makeLenses ''Map

--data MapST s = MapST
--  { _cellsST :: STArray s (Int, Int) MapCell }
--makeLenses ''MapST

data Level = Level
  { _map :: Map
  }
makeLenses ''Level

data Environment =
  Environment { _player :: Player, _mobs :: [Player], _levels :: [Level] }

