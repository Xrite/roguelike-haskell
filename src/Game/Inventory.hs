{-# LANGUAGE TemplateHaskell #-}
module Game.Inventory where

import Control.Lens 
import Game.Item

data Inventory = Inventory { _items :: [Item], _slots :: Slots}

data Slots =
  Slots { _head :: Maybe Item, _chest :: Maybe Item, _legs :: Maybe Item }

makeLenses ''Inventory
makeLenses ''Slots


