{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Unit.Unit
  ( UnitData
  , stats
  , position
  , _position
  , timedEffects
  , inventory
  , baseWeapon
  , Unit(..)
  , AnyUnit(..)
  , packUnit
  , anyUnitAsData
  , anyUnitApplyEffect
  , Action
  , createUnitData
  , position
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
      { _position :: (Int, Int),
        _depth :: Int,
        _stats :: Stats,
        _timedEffects :: TimedEffects,
        _inventory :: Inventory,
        _baseWeapon :: WeaponItem,
        _control :: GameLevel -> [AnyUnit] -> GameIO Action
      }

createUnitData
  :: (Int, Int)
  -> Int
  -> Stats
  -> TimedEffects
  -> Inventory
  -> WeaponItem
  -> (GameLevel -> [AnyUnit] -> GameIO Action)
  -> UnitData
createUnitData = UnitData 

-- | Something that can hit and run
class Unit a where
  asUnitData :: a -> UnitData
  applyEffect :: Effect () -> a -> a
  attackEffect :: a -> Effect ()

data AnyUnit = forall a. (Unit a) => AnyUnit a

makeLenses ''UnitData

packUnit :: Unit a => a -> AnyUnit
packUnit = AnyUnit 

anyUnitAsData :: AnyUnit -> UnitData
anyUnitAsData (AnyUnit u) = asUnitData u

anyUnitApplyEffect :: Effect () -> AnyUnit -> AnyUnit
anyUnitApplyEffect effect (AnyUnit u) = AnyUnit $ applyEffect effect u