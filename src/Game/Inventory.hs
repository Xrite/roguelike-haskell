{-# LANGUAGE TemplateHaskell #-}

module Game.Inventory
  ( Inventory,
    WearableSlots,
    items,
    wearableSlots,
    weaponSlots,
    headSlot,
    chestSlot,
    legsSlot,
    getAllWearableEffects,
    getWeaponEffect
  )
where

import Control.Lens
import Game.Effect
import Game.Item
import Prelude hiding (head)

data Inventory
  = Inventory
      { _items :: [Item],
        _wearableSlots :: WearableSlots,
        _weaponSlots :: WeaponSlots
      }

data WearableSlots
  = WearableSlots
      { _headSlot :: Maybe WearableItem,
        _chestSlot :: Maybe WearableItem,
        _legsSlot :: Maybe WearableItem
      }

data WeaponSlots = WeaponSlots {_hand :: Maybe WeaponItem}

makeLenses ''Inventory

makeLenses ''WearableSlots

makeLenses ''WeaponSlots

getWearableEffect :: Maybe WearableItem -> Effect ()
getWearableEffect (Just item) = item ^. wearableDefenceEffect
getWearableEffect Nothing = return ()

getAllWearableEffects :: WearableSlots -> Effect ()
getAllWearableEffects slots = do
  getWearableEffect $ slots ^. headSlot
  getWearableEffect $ slots ^. chestSlot
  getWearableEffect $ slots ^. legsSlot

getWeaponEffect :: WeaponSlots -> Effect ()
getWeaponEffect slots = case slots ^. hand of
  (Just item) -> item ^. weaponAttackEffect
  Nothing -> return ()