{-# LANGUAGE TemplateHaskell #-}

module Game.Unit.Inventory
  ( Inventory
  , WearableSlots
  , InventoryError(..)
  , items
  , wearableSlots
  , weaponSlots
  , headSlot
  , chestSlot
  , legsSlot
  , getAllWearableEffects
  , getWeaponEffect
  , emptyInventory
  , addItem
  , fillHeadSlot
  , fillChestSlot
  , fillLegsSlot
  , fillWeaponSlot
  )
where

import           Control.Lens
import           Game.Effect
import           Game.Item
import           Prelude                 hiding ( head )

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

data InventoryError = Occupied | WrongItemType | WrongWearableType

makeLenses ''Inventory

makeLenses ''WearableSlots

makeLenses ''WeaponSlots

getWearableEffect :: Maybe WearableItem -> Effect ()
getWearableEffect (Just item) = wearableDefenceEffect item
getWearableEffect Nothing     = return ()

getAllWearableEffects :: Inventory -> Effect ()
getAllWearableEffects inv = do
  let slots = inv ^. wearableSlots
  getWearableEffect $ slots ^. headSlot
  getWearableEffect $ slots ^. chestSlot
  getWearableEffect $ slots ^. legsSlot

getWeaponEffect :: Inventory -> Effect ()
getWeaponEffect inv = case inv ^. weaponSlots . hand of
  (Just item) -> item ^. weaponAttackEffect
  Nothing     -> return ()

emptyInventory :: Inventory
emptyInventory =
  Inventory [] (WearableSlots Nothing Nothing Nothing) (WeaponSlots Nothing)

addItem :: Item -> Inventory -> Inventory
addItem item = over items (item :)

fillHeadSlot :: Inventory -> Item -> Either InventoryError Inventory
fillHeadSlot inv item = case toWearable item of
  Nothing    -> Left WrongItemType
  Just wItem -> case inv ^. wearableSlots . headSlot of
    Just _  -> Left Occupied
    Nothing -> case wearableType wItem of
      Head -> Right $ inv & wearableSlots . headSlot .~ pure wItem
      _    -> Left WrongWearableType

fillChestSlot :: Inventory -> Item -> Either InventoryError Inventory
fillChestSlot inv item = case toWearable item of
  Nothing    -> Left WrongItemType
  Just wItem -> case inv ^. wearableSlots . chestSlot of
    Just _  -> Left Occupied
    Nothing -> case wearableType wItem of
      Chest -> Right $ inv & wearableSlots . chestSlot .~ pure wItem
      _     -> Left WrongWearableType

fillLegsSlot :: Inventory -> Item -> Either InventoryError Inventory
fillLegsSlot inv item = case toWearable item of
  Nothing    -> Left WrongItemType
  Just wItem -> case inv ^. wearableSlots . legsSlot of
    Just _  -> Left Occupied
    Nothing -> case wearableType wItem of
      Legs -> Right $ inv & wearableSlots . legsSlot .~ pure wItem
      _    -> Left WrongWearableType

fillWeaponSlot :: Inventory -> Item -> Either InventoryError Inventory
fillWeaponSlot inv item = case toWeapon item of
  Nothing -> Left WrongItemType
  Just wItem -> case inv ^. weaponSlots . hand of
    Just _ -> Left Occupied
    Nothing -> Right $ inv & weaponSlots . hand .~ pure wItem