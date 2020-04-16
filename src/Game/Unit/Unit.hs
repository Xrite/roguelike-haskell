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
    timedModifiers,
    inventory,
    baseWeapon,
    Unit (..),
    AnyUnit (..),
    packUnit,
    Action (..),
    Direction (..),
    createUnitData,
    position,
    getPosition,
  )
where

import Control.Lens
import Data.Maybe (fromMaybe)
import Game.GameLevels.GameLevel
import Game.IO.GameIO
import Game.Item
import Game.Modifiers.EffectDesc (EffectDesc)
import Game.Modifiers.Modifier
import Game.Modifiers.ModifierFactory
import Game.Unit.Action
import Game.Unit.Inventory
import Game.Unit.Stats
import Game.Unit.TimedModifiers

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
        _timedModifiers :: TimedModifiers,
        -- | Inventory on a unit
        _inventory :: Inventory,
        -- | A weapon to use when unit is fighting bare-hand TODO use it in calculations
        _baseWeapon :: WeaponItem,
        -- | How to display this unit
        -- | Defines behavior of a unit. Arguments are level and all the other units on it. TODO remove if not used
        _portrait :: Char,
        _control :: GameLevel -> [AnyUnit] -> GameIO Action
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
  TimedModifiers ->
  -- | Inventory on a unit
  Inventory ->
  -- | A weapon to use when unit is fighting bare-hand TODO use it in calculations
  WeaponItem ->
  -- | How to display this unit
  Char ->
  -- | Defines behavior of a unit. Arguments are level and all the other units on it.
  (GameLevel -> [AnyUnit] -> GameIO Action) ->
  -- | Constructed 'Unit'
  UnitData
createUnitData = UnitData

-- | Returns an active weapon unit data implies.
-- That is, returns equipped weapon or base weapon if none equipped
getWeapon :: UnitData -> WeaponItem
getWeapon unitData = fromMaybe (_baseWeapon unitData) (getEquippedWeapon $ _inventory unitData)

-- | Returns attack modifier this unit data implies
getAttackModifier :: UnitData -> EffectDesc
getAttackModifier unitData = getWeapon unitData ^. weaponAttackModifier

-- | Something that can hit and run.
-- A typeclass for every active participant of a game. If it moves and participates in combat system, it is a unit.
class Unit u where
  -- | Returns 'UnitData' of a unit.
  asUnitData :: a -> UnitData

  -- | How unit is affected by 'Modifier's.
  -- It is the main thing that differs a 'Unit' from 'UnitData'.
  applyModifier :: Modifier () -> a -> a

  -- | Modifier for this unit's attacks
  attackModifier :: ModifierFactory -> a -> Modifier ()
  attackModifier factory p = buildModifier factory $ getAttackModifier . asUnitData $ applyModifier (buildModifier factory wearableEff) p
    where
      inv = _inventory $ asUnitData p
      wearableEff = getAllWearableModifiers inv

-- | An existential type wrapper for any type implementing 'Unit'.
-- Existential classes is an antipattern, but what other choice do we have?
data AnyUnit = forall a. (Unit a) => AnyUnit a

makeLenses ''UnitData

position :: UnitData -> (Int, Int)
position = _positionLens

depth :: UnitData -> Int
depth = _depthLens

stats :: UnitData -> Stats.Stats
stats = _statsLens

timedEffects :: UnitData -> TimedEffects.TimedEffects
timedEffects = _timedEffectsLens

inventory :: UnitData -> Inventory.Inventory
inventory = _inventoryLens

baseWeapon :: UnitData -> Item.WeaponItem
baseWeapon = _baseWeaponLens

-- | Instance of 'Unit' for the wrapper 'AnyUnit" that simply transfers every call to the wrapped object.
instance Unit AnyUnit where
  asUnitData (AnyUnit u) = asUnitData u
  applyModifier e (AnyUnit u) = AnyUnit $ applyModifier e u
  attackModifier fact (AnyUnit u) = attackModifier fact u

-- | Packs any unit into 'AnyUnit' box.
packUnit :: Unit a => a -> AnyUnit
packUnit = AnyUnit

getPosition :: Unit a => a -> (Int, Int)
getPosition u = asUnitData u ^. positionLens
