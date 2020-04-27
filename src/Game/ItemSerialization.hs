module Game.ItemSerialization () where

import Data.Binary (Binary)
import Game.Modifiers.EffectSerialization ()
import Game.Item


instance Binary WearableType
instance Binary Item
instance Binary WeaponItem
instance Binary WearableItem
instance Binary ConsumableItem
instance Binary JunkItem