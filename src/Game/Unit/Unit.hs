{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Describes common interface for all units in the game.
module Game.Unit.Unit
  ( UnitData,
    stats,
    _stats,
    position,
    _position,
    _portrait,
    timedUnitOps,
    inventory,
    baseWeapon,
    Unit (..),
    Action (..),
    Direction (..),
    AnyUnit,
    packUnit,
    createUnitData,
    getPosition,
    isAlive,
    getAttackUnitOp,
    applyUnitOp_,
    portrait,
  )
where

import Control.Lens
import Data.Maybe (fromMaybe)
import Game.Item
import Game.Modifiers.EffectDesc (EffectDesc)
import Game.Modifiers.UnitOp
import Game.Unit.Action
import Game.Unit.Inventory
import Game.Unit.Stats
import Game.Unit.TimedUnitOps

-- | Common data of all units.
data UnitData
  = UnitData
      { -- | Coordinates on a level
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
        -- | Defines behavior of a unit. Arguments are level and all the other units on it. TODO remove if not used
        _portrait :: Char
      }

-- | Constructs a new 'UnitData'.
createUnitData ::
  -- | Coordinates on a level
  (Int, Int) ->
  -- | Level (as in depth) on which the unit is now
  Int ->
  -- | Stats of a unit
  Stats ->
  -- | Timed modifiers that are affecting the unit
  TimedUnitOps ->
  -- | Inventory on a unit
  Inventory ->
  -- | A weapon to use when unit is fighting bare-hand TODO use it in calculations
  WeaponItem ->
  -- | How to display this unit
  Char ->
  -- | Constructed 'Unit'
  UnitData
createUnitData = UnitData

makeLenses ''UnitData

-- | Returns an active weapon unit data implies.
-- That is, returns equipped weapon or base weapon if none equipped
getWeapon :: UnitData -> WeaponItem
getWeapon unitData = fromMaybe (_baseWeapon unitData) (getEquippedWeapon $ _inventory unitData)

-- | Returns attack modifier this unit data implies
getAttackUnitOp :: UnitData -> EffectDesc
getAttackUnitOp unitData = getWeapon unitData ^. weaponAttackUnitOp

-- | Something that can hit and run.
-- A typeclass for every active participant of a game. If it moves and participates in combat system, it is a unit.
class Unit u where
  -- | Returns 'UnitData' of a unit.
  asUnitData :: u -> UnitData

  -- | How unit is affected by 'UnitOp's.
  -- It is the main thing that differs a 'Unit' from 'UnitData'.
  applyUnitOp :: UnitOp a -> u -> (u, a)

applyUnitOp_ :: Unit u => UnitOp a -> u -> u
applyUnitOp_ modifier u = fst $ applyUnitOp modifier u

isAlive :: Unit u => u -> Bool
isAlive u = asUnitData u ^. stats . health > 0

data AnyUnit = forall a. Unit a => AnyUnit a

packUnit :: (Unit u) => u -> AnyUnit
packUnit = AnyUnit

instance Unit AnyUnit where
  asUnitData (AnyUnit u) = asUnitData u
  applyUnitOp modifier (AnyUnit u) = over _1 AnyUnit $ applyUnitOp modifier u
