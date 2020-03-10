{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Unit
  ( UnitData
  , stats
  , timedEffects
  , inventory
  , baseWeapon
  , Unit(..)
  , AnyUnit
  , packUnit
  )
where

import           Control.Lens
import           Game.Effect
import           Game.Inventory
import           Game.IO.GameIO
import           Game.GameLevels.GameLevel
import           Game.Item
import           Game.Stats
import           Game.TimedEffects

data Direction = Positive
               | Negative
               | Zero

data Action = Move Direction Direction

data UnitData
  = UnitData
      { _stats :: Stats,
        _timedEffects :: TimedEffects,
        _inventory :: Inventory,
        _baseWeapon :: WeaponItem,
        _control :: GameLevel -> [Unit] -> GameIO Action
      }

makeLenses ''UnitData

-- | Something that can hit and run
class Unit a where
  asUnitData :: a -> UnitData
  applyEffect :: Effect () -> a -> a

data AnyUnit = forall a. Unit a => AnyUnit a

packUnit :: Unit a => a -> AnyUnit
packUnit unit = AnyUnit unit
