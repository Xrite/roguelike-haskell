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
    hand,
    getAllWearableUnitOps,
    emptyInventory,
    addItem,
    fillHeadSlot,
    fillChestSlot,
    fillLegsSlot,
    fillWeaponSlot,
    getEquippedWeapon,
    freeHeadSlot,
    freeChestSlot,
    freeLegsSlot,
    freeHandSlot,
    tryEquipItem,
  )
where

import Control.Applicative
import Control.Lens
import Data.Either
import Data.Maybe
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
      deriving (Generic, Eq)

data WearableSlots
  = WearableSlots
      { _headSlot :: Maybe WearableItem,
        _chestSlot :: Maybe WearableItem,
        _legsSlot :: Maybe WearableItem
      }
      deriving (Generic, Eq)

newtype WeaponSlots = WeaponSlots{_hand :: Maybe WeaponItem}
                        deriving (Generic, Eq)

data InventoryError = Occupied | WrongItemType | WrongWearableType | NoSuchItem
      deriving (Generic, Eq)

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

-- | Try to fill suitable slot with given item
tryFillSlot :: Inventory -> Item -> Either InventoryError Inventory
tryFillSlot inv item =
  case rights [fillHeadSlot inv item, fillChestSlot inv item, fillLegsSlot inv item, fillWeaponSlot inv item] of
    [] -> Left Occupied
    x : _ -> Right x

-- | Equips an item by index or returns an error
tryEquipItem :: Int -> Inventory -> Either InventoryError Inventory
tryEquipItem i inv = do
  let itms = _items inv
  if i < 0 || length itms <= i
    then Left NoSuchItem
    else return ()
  inv' <- tryFillSlot inv (itms !! i)
  return $ items %~ pop i $ inv'
  where
    pop i list = take i list ++ drop (i + 1) list


freeHeadSlot :: Inventory -> Inventory
freeHeadSlot inv
  | Just item <- slot = set (wearableSlots . headSlot) Nothing $ (addItem (wearableToItem item) inv)
  | otherwise = inv
  where
    slot = inv ^. wearableSlots . headSlot

freeChestSlot :: Inventory -> Inventory
freeChestSlot inv
  | Just item <- slot = set (wearableSlots . chestSlot) Nothing $ (addItem (wearableToItem item) inv)
  | otherwise = inv
  where
    slot = inv ^. wearableSlots . chestSlot

freeLegsSlot :: Inventory -> Inventory
freeLegsSlot inv
  | Just item <- slot = set (wearableSlots . legsSlot) Nothing $ (addItem (wearableToItem item) inv)
  | otherwise = inv
  where
    slot = inv ^. wearableSlots . legsSlot

freeHandSlot :: Inventory -> Inventory
freeHandSlot inv
  | Just item <- slot = set (weaponSlots . hand) Nothing $ (addItem (weaponToItem item) inv)
  | otherwise = inv
  where
    slot = inv ^. weaponSlots . hand
