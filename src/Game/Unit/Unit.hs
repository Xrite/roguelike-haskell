{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Describes common interface for all units in the game.
module Game.Unit.Unit
    ( UnitData
    , depth
    , depthLens
    , stats
    , statsLens
    , position
    , positionLens
    , timedEffects
    , timedEffectsLens
    , inventory
    , inventoryLens
    , baseWeapon
    , Unit(..)
    , AnyUnit(..)
    , packUnit
    , createUnitData
    , getPosition) where

import           Control.Lens
import qualified Game.Effect as Effect
import qualified Game.Unit.Inventory as Inventory
import           Game.IO.GameIO
import qualified Game.GameLevels.GameLevel as GameLevel
import qualified Game.Item as Item
import qualified Game.Unit.Stats as Stats
import qualified Game.Unit.TimedEffects as TimedEffects
import qualified Game.Unit.Action as Action
import qualified Game.Scenario as Scenario

-- | Common data of all units.
data UnitData =
  UnitData { _positionLens :: (Int, Int)                               -- ^ Coordinates on a level
           , _depthLens :: Int                                         -- ^ Level (as in depth) on which the unit is now
           , _statsLens :: Stats.Stats                                       -- ^ Stats of a unit
           , _timedEffectsLens :: TimedEffects.TimedEffects                         -- ^ Timed effects that are affecting the unit
           , _inventoryLens :: Inventory.Inventory                               -- ^ Inventory on a unit
           , _baseWeaponLens :: Item.WeaponItem                             -- ^ A weapon to use when unit is fighting bare-hand TODO use it in calculations
             -- | Defines behavior of a unit. Arguments are level and all the other units on it. TODO remove if not used
           , control
               :: GameLevel.GameLevel -> [AnyUnit] -> GameIO Action.Action
           }

-- | Constructs a new 'UnitData'.
createUnitData
  :: (Int, Int)                                 -- ^ Coordinates on a level
  -> Int                                        -- ^ Level (as in depth) on which the unit is now
  -> Stats.Stats                                -- ^ Stats of a unit
  -> TimedEffects.TimedEffects                  -- ^ Timed effects that are affecting the unit
  -> Inventory.Inventory                        -- ^ Inventory on a unit
  -> Item.WeaponItem                            -- ^ A weapon to use when unit is fighting bare-hand TODO use it in calculations
  -> (GameLevel.GameLevel
      -> [AnyUnit]
      -> GameIO Action.Action)                  -- ^ Defines behavior of a unit. Arguments are level and all the other units on it.
  -> UnitData                                   -- ^ Constructed 'Unit'

createUnitData = UnitData

-- | Something that can hit and run.
-- A typeclass for every active participant of a game. If it moves and participates in combat system, it is a unit.
class Unit u where
  -- | Returns 'UnitData' of a unit.
  asUnitData :: u -> UnitData

  -- | How unit is affected by 'Effect's.
  -- It is the main thing that differs a 'Unit' from 'UnitData'.
  applyEffect :: Effect.Effect a -> u -> (u, a)

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

  applyEffect e (AnyUnit u) = (AnyUnit newUnit, result)
    where
      (newUnit, result) = applyEffect e u

-- | Packs any unit into 'AnyUnit' box.
packUnit :: Unit a => a -> AnyUnit
packUnit = AnyUnit

getPosition :: Unit a => a -> (Int, Int)
getPosition u = asUnitData u ^. positionLens
