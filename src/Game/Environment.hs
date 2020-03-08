module Game.Environment where

data Environment =
  Environment { _player :: Player, _mobs :: [Player], _levels :: [Level] }

data Player =
  Player { _position :: (Int, Int), _stats :: Stats, _inventory :: Inventory }

data Stats = Stats { _health :: Int, _level :: Int }

data Inventory = Inventory { _items :: [Item] }

data Item = Item { _name :: String }

data Level = Level { _map :: Map }

data Map = Map { _cells :: [[MapCell]]}

data MapCell = MapCell { _render :: Char }



