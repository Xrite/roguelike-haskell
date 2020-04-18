{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Describes common interface for all units in the game.
module Game.Unit.Unit
  ( UnitData
  , stats
  , _stats
  , position
  , _position
  , _portrait
  , timedUnitOps
  , inventory
  , baseWeapon
  , Unit(..)
  , AnyUnit(..)
  , packUnit
  , Action(..)
  , Direction(..)
  , createUnitData
  , position
  , getPosition
  )
where

import           Control.Lens
import           Game.Modifiers.UnitOp
import           Game.Unit.Inventory
import           Game.IO.GameIO
import           Game.GameLevels.GameLevel
import           Game.Item
import           Game.Unit.Stats
import           Game.Unit.TimedUnitOps
import           Game.Unit.Action
import Data.Maybe (fromMaybe)
import Game.Modifiers.EffectDesc (EffectDesc)
import Game.Modifiers.UnitOpFactory

-- | Common data of all units.
data UnitData
  = UnitData
      { _position :: (Int, Int),                              -- ^ Coordinates on a level
        _depth :: Int,                                        -- ^ Level (as in depth) on which the unit is now
        _stats :: Stats,                                      -- ^ Stats of a unit
        _timedUnitOps :: TimedUnitOps,                        -- ^ Timed modifiers that are affecting the unit
        _inventory :: Inventory,                              -- ^ Inventory on a unit
        _baseWeapon :: WeaponItem,                            -- ^ A weapon to use when unit is fighting bare-hand TODO use it in calculations
        _portrait :: Char,                                    -- ^ How to display this unit
        -- | Defines behavior of a unit. Arguments are level and all the other units on it. TODO remove if not used
        _control :: GameLevel -> [AnyUnit] -> GameIO Action
      }

-- | Constructs a new 'UnitData'.
createUnitData
  :: (Int, Int)                                 -- ^ Coordinates on a level
  -> Int                                        -- ^ Level (as in depth) on which the unit is now
  -> Stats                                      -- ^ Stats of a unit
  -> TimedUnitOps                               -- ^ Timed modifiers that are affecting the unit
  -> Inventory                                  -- ^ Inventory on a unit
  -> WeaponItem                                 -- ^ A weapon to use when unit is fighting bare-hand TODO use it in calculations
  -> Char                                       -- ^ How to display this unit
  -> (GameLevel -> [AnyUnit] -> GameIO Action)  -- ^ Defines behavior of a unit. Arguments are level and all the other units on it.
  -> UnitData                                   -- ^ Constructed 'Unit'
createUnitData = UnitData

-- | Returns an active weapon unit data implies.
-- That is, returns equipped weapon or base weapon if none equipped
getWeapon :: UnitData -> WeaponItem
getWeapon unitData = fromMaybe (_baseWeapon unitData) (getEquippedWeapon $ _inventory unitData)

-- | Returns attack modifier this unit data implies
getAttackUnitOp :: UnitData -> EffectDesc
getAttackUnitOp unitData = getWeapon unitData ^. weaponAttackUnitOp

-- | Something that can hit and run.
-- A typeclass for every active participant of a game. If it moves and participates in combat system, it is a unit.
class Unit a where
  -- | Returns 'UnitData' of a unit.
  asUnitData :: a -> UnitData
  -- | How unit is affected by 'UnitOp's.
  -- It is the main thing that differs a 'Unit' from 'UnitData'.
  applyUnitOp :: UnitOp () -> a -> a
  -- | UnitOp for this unit's attacks
  attackUnitOp :: UnitOpFactory -> a -> UnitOp ()
  attackUnitOp factory p = buildUnitOp factory $ getAttackUnitOp . asUnitData $ applyUnitOp (buildUnitOp factory wearableEff) p
      where
        inv = _inventory $ asUnitData p
        wearableEff = getAllWearableUnitOps inv

-- | An existential type wrapper for any type implementing 'Unit'.
-- Existential classes is an antipattern, but what other choice do we have? 
data AnyUnit = forall a. (Unit a) => AnyUnit a

makeLenses ''UnitData

-- | Instance of 'Unit' for the wrapper 'AnyUnit" that simply transfers every call to the wrapped object.
instance Unit AnyUnit where
  asUnitData (AnyUnit u) = asUnitData u
  applyUnitOp e (AnyUnit u) = AnyUnit $ applyUnitOp e u
  attackUnitOp fact (AnyUnit u) = attackUnitOp fact u

-- | Packs any unit into 'AnyUnit' box.
packUnit :: Unit a => a -> AnyUnit
packUnit = AnyUnit 

getPosition :: Unit a => a -> (Int, Int)
getPosition u = asUnitData u ^. position