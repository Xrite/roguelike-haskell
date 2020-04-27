{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Game.Unit.Inventory
  ( Inventory,
    WearableSlots,
    WeaponSlots,
    InventoryError (..),
    items,
    wearableSlots,
    weaponSlots,
    headSlot,
    chestSlot,
    legsSlot,
    getAllWearableUnitOps,
    emptyInventory,
    addItem,
    fillHeadSlot,
    fillChestSlot,
    fillLegsSlot,
    fillWeaponSlot,
    getEquippedWeapon,
  )
where

import Control.Lens
import Game.Item
import Game.Modifiers.EffectDesc
import Prelude hiding (head)
import GHC.Generics (Generic)

data Inventory
  = Inventory
      { _items :: [Item],
        _wearableSlots :: WearableSlots,
        _weaponSlots :: WeaponSlots
      }
      deriving (Generic)

data WearableSlots
  = WearableSlots
      { _headSlot :: Maybe WearableItem,
        _chestSlot :: Maybe WearableItem,
        _legsSlot :: Maybe WearableItem
      }
      deriving (Generic)

newtype WeaponSlots = WeaponSlots{_hand :: Maybe WeaponItem}
                        deriving (Generic)

data InventoryError = Occupied | WrongItemType | WrongWearableType
      deriving (Generic)

makeLenses ''Inventory

makeLenses ''WearableSlots

makeLenses ''WeaponSlots

-- | Get modifier from wearable item
getWearableUnitOp :: Maybe WearableItem -> EffectDesc
getWearableUnitOp (Just item) = wearableDefenceUnitOp item
getWearableUnitOp Nothing = pure ()

-- | Get composite modifier from all equipped wearable items in an inventory
getAllWearableUnitOps :: Inventory -> EffectDesc
getAllWearableUnitOps inv = do
  let slots = inv ^. wearableSlots
  getWearableUnitOp $ slots ^. headSlot
  getWearableUnitOp $ slots ^. chestSlot
  getWearableUnitOp $ slots ^. legsSlot

-- | Get an equipped weapon (if there is one)
getEquippedWeapon :: Inventory -> Maybe WeaponItem
getEquippedWeapon inv = inv ^. weaponSlots . hand

-- | Empty inventory
emptyInventory :: Inventory
emptyInventory =
  Inventory [] (WearableSlots Nothing Nothing Nothing) (WeaponSlots Nothing)

-- | Add item to the inventory
addItem :: Item -> Inventory -> Inventory
addItem item = over items (item :)

-- | Try to equip item to the head slot, this action can fail, in that case it returns error type
fillHeadSlot :: Inventory -> Item -> Either InventoryError Inventory
fillHeadSlot inv item = case toWearable item of
  Nothing -> Left WrongItemType
  Just wItem -> case inv ^. wearableSlots . headSlot of
    Just _ -> Left Occupied
    Nothing -> case wearableType wItem of
      Head -> Right $ inv & wearableSlots . headSlot .~ pure wItem
      _ -> Left WrongWearableType

-- | Try to equip item to the chest slot, this action can fail, in that case it returns error type
fillChestSlot :: Inventory -> Item -> Either InventoryError Inventory
fillChestSlot inv item = case toWearable item of
  Nothing -> Left WrongItemType
  Just wItem -> case inv ^. wearableSlots . chestSlot of
    Just _ -> Left Occupied
    Nothing -> case wearableType wItem of
      Chest -> Right $ inv & wearableSlots . chestSlot .~ pure wItem
      _ -> Left WrongWearableType

-- | Try to equip item to the legs slot, this action can fail, in that case it returns error type
fillLegsSlot :: Inventory -> Item -> Either InventoryError Inventory
fillLegsSlot inv item = case toWearable item of
  Nothing -> Left WrongItemType
  Just wItem -> case inv ^. wearableSlots . legsSlot of
    Just _ -> Left Occupied
    Nothing -> case wearableType wItem of
      Legs -> Right $ inv & wearableSlots . legsSlot .~ pure wItem
      _ -> Left WrongWearableType

-- | Try to equip item to the weapon slot, this action can fail, in that case it returns error type
fillWeaponSlot :: Inventory -> Item -> Either InventoryError Inventory
fillWeaponSlot inv item = case toWeapon item of
  Nothing -> Left WrongItemType
  Just wItem -> case inv ^. weaponSlots . hand of
    Just _ -> Left Occupied
    Nothing -> Right $ inv & weaponSlots . hand .~ pure wItem