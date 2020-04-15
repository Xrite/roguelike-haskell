{-# LANGUAGE TemplateHaskell #-}

module Game.Item
  ( WearableType(..)
  , Item
  , WeaponItem
  , WearableItem
  , ConsumableItem
  , JunkItem
  , name
  , itemRender
  , createWeapon
  , createWearable
  , createConsumable
  , createJunk
  , weaponToItem
  , wearableToItem
  , consumableToItem
  , junkToItem
  , wearableType
  , wearableDefenceEffect
  , wearableRepulseEffect
  , consumableEffect
  , weaponAttackEffect
  , toWearable
  , toWeapon
  , toConsumable
  , toJunk
  )
  
where

import           Game.Effects.Modifier
import           Control.Lens

data WearableType = Head
                  | Chest
                  | Legs

data Item = Consumable ConsumableItem
          | Wearable WearableItem
          | Weapon WeaponItem
          | Junk JunkItem

data WeaponItem =
  WeaponItem { _weaponName :: String, _weaponAttackEffect :: Modifier () , _weaponRender :: Char}

data WearableItem = WearableItem { _wearableName :: String
                                 , wearableType :: WearableType
                                 , wearableDefenceEffect :: Modifier ()
                                 , wearableRepulseEffect :: Modifier ()
                                 , _wearableRender :: Char
                                 }

data ConsumableItem =
  ConsumableItem { _consumableName :: String, _consumableEffect :: Modifier () , _consumableRender :: Char}

data JunkItem = JunkItem { _junkName :: String, _junkRender :: Char}

makeLenses ''WeaponItem
makeLenses ''WearableItem
makeLenses ''ConsumableItem
makeLenses ''JunkItem

createWeapon :: String -> Modifier () -> Char -> WeaponItem
createWeapon = WeaponItem

createWearable
  :: String -> WearableType -> Modifier () -> Modifier () -> Char -> WearableItem
createWearable = WearableItem

createConsumable :: String -> Modifier () -> Char -> ConsumableItem
createConsumable = ConsumableItem

weaponToItem :: WeaponItem -> Item
weaponToItem = Weapon

wearableToItem :: WearableItem -> Item
wearableToItem = Wearable

consumableToItem :: ConsumableItem -> Item
consumableToItem = Consumable

junkToItem :: JunkItem -> Item
junkToItem = Junk

createJunk :: String -> Char -> JunkItem
createJunk = JunkItem

toWearable :: Item -> Maybe WearableItem
toWearable (Wearable item) = Just item
toWearable _ = Nothing 

toWeapon :: Item -> Maybe WeaponItem
toWeapon (Weapon item) = Just item
toWeapon _ = Nothing 

toConsumable :: Item -> Maybe ConsumableItem
toConsumable (Consumable item) = Just item
toConsumable _ = Nothing 

toJunk :: Item -> Maybe JunkItem
toJunk (Junk item) = Just item
toJunk _ = Nothing 

name :: Item -> String
name (Consumable item) = _consumableName item
name (Wearable   item) = _wearableName item
name (Weapon     item) = _weaponName item
name (Junk       item) = _junkName item

-- anton make this plz
itemRender :: Item -> Char
itemRender (Consumable item) = _consumableRender item
itemRender (Wearable   item) = _wearableRender item
itemRender (Weapon     item) = _weaponRender item
itemRender (Junk       item) = _junkRender item