{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Unit.Unit
  ( UnitData
  , stats
  , timedEffects
  , inventory
  , baseWeapon
  , Unit(..)
  , AnyUnit
  , packUnit
  , Action
  , createUnitData
  )
where

import           Control.Lens
import           Game.Effect
import           Game.Unit.Inventory
import           Game.IO.GameIO
import           Game.GameLevels.GameLevel
import           Game.Item
import           Game.Unit.Stats
import           Game.Unit.TimedEffects

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
        _control :: GameLevel -> [AnyUnit] -> GameIO Action
      }

createUnitData
  :: Stats
  -> TimedEffects
  -> Inventory
  -> WeaponItem
  -> (GameLevel -> [AnyUnit] -> GameIO Action)
  -> UnitData
createUnitData stats effs inv bw ai = UnitData stats effs inv bw ai


-- | Something that can hit and run
class Unit a where
  asUnitData :: a -> UnitData
  applyEffect :: Effect () -> a -> a
  createAttackEffect :: a -> Effect ()
  createRepulseEffect :: a -> Effect ()

data AnyUnit = forall a. Unit a => AnyUnit a

makeLenses ''UnitData

packUnit :: Unit a => a -> AnyUnit
packUnit unit = AnyUnit unit
