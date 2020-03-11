{-# LANGUAGE TemplateHaskell #-}

module Game.Item
    ( WearableType
    , Item
    , WeaponItem
    , WearableItem
    , ConsumableItem
    , wearableDefenceEffect
    , wearableRepulseEffect
    , weaponAttackEffect
    , name
    , itemRender) where

import           Game.Effect
import           Control.Lens

data WearableType = Head
                  | Chest
                  | Legs

data Item = Consumable ConsumableItem
          | Wearable WearableItem
          | Weapon WeaponItem
          | Junk String

data WeaponItem =
  WeaponItem { _weaponName :: String, _weaponAttackEffect :: Effect () }

data WearableItem = WearableItem { _wearableName :: String
                                 , _wearableType :: WearableType
                                 , _wearableDefenceEffect :: Effect ()
                                 , _wearableRepulseEffect :: Effect ()
                                 }

data ConsumableItem =
  ConsumableItem { _consumableName :: String, _consumableEffect :: Effect () }

data JunkItem = JunkItem { _junkName :: String }

makeLenses ''WeaponItem
makeLenses ''WearableItem
makeLenses ''ConsumableItem
makeLenses ''JunkItem

name :: Item -> String
name (Consumable item) = _consumableName item
name (Wearable item) = _wearableName item
name (Weapon item) = _weaponName item
name (Junk str) = str

-- anton make this plz
itemRender :: Item -> Char
itemRender = undefined 
