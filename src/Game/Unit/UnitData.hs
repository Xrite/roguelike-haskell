{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Game.Unit.UnitData
  ( UnitData (..),
    confused,
    position,
    depth,
    stats,
    timedUnitOps,
    inventory,
    baseWeapon,
    portrait,
    getWeapon,
    getAttackUnitOp
) where


import Control.Lens (makeLenses, (^.))
import Game.Unit.Stats (Stats)
import Game.Unit.TimedUnitOps (TimedUnitOps)
import Game.Unit.Inventory (Inventory, getEquippedWeapon)
import Game.Item (WeaponItem, weaponAttackUnitOp)
import Game.Modifiers.EffectDesc (EffectDesc)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
 
-- | Common data of all units.
data UnitData
  = UnitData
      { -- | If unit is confused
        _confused :: Bool,
        -- | Coordinates on a level
        _position :: (Int, Int),
        -- | Level (as in depth) on which the unit is now
        _depth :: Int,
        -- | Stats of a unit
        _stats :: Stats,
        -- | Timed modifiers that are affecting the unit
        _timedUnitOps :: TimedUnitOps,
        -- | Inventory on a unit
        _inventory :: Inventory,
        -- | A weapon to use when unit is fighting bare-hand TODO use it in calculations
        _baseWeapon :: WeaponItem,
        -- | How to display this unit
        _portrait :: Char
      }
      deriving (Generic, Eq)

makeLenses ''UnitData
-- | Returns an active weapon unit data implies.
-- That is, returns equipped weapon or base weapon if none equipped
getWeapon :: UnitData -> WeaponItem
getWeapon unitData = fromMaybe (_baseWeapon unitData) (getEquippedWeapon $ _inventory unitData)

-- | Returns attack modifier this unit data implies
getAttackUnitOp :: UnitData -> EffectDesc
getAttackUnitOp unitData = getWeapon unitData ^. weaponAttackUnitOp

