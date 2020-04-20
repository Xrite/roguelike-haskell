{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Describes common interface for all units in the game.
module Game.Unit.Unit
  ( UnitData(..),
    confused,
    position,
    depth,
    stats,
    timedUnitOps,
    inventory,
    baseWeapon,
    portrait,
    createUnitData,
    AnyUnit (..),
    LevellingStats,
    experience,
    skillPoints,
    Player,
    playerUnit,
    levelling,
    Mob,
    mobUnit,
    strategy,
    Unit (..),
    applyUnitOp_,
    getWeapon,
    getAttackUnitOp,
    isAlive,
    makePlayer,
    makeMob,
  )
where

import Control.Lens
import Control.Monad.Free
import Data.Maybe (fromMaybe)
import Game.Item
import Game.Modifiers.EffectAtom
import Game.Modifiers.EffectDesc (EffectDesc)
import Game.Modifiers.UnitOp
import Game.Unit.Action
import Game.Unit.Inventory
import Game.Unit.Stats
import Game.Unit.TimedUnitOps

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

-- | Tagged union of units
data AnyUnit ctx = MkMob (Mob ctx) | MkPlayer Player

data LevellingStats
  = LevellingStats {_experience :: Int, _skillPoints :: Int}

-- | A unit that can get experience points and level-ups. Controlled from the outside world.
data Player = Player {_playerUnit :: UnitData, _levelling :: LevellingStats}

-- | A mob is a simple computer-controlled 'Unit'.
data Mob ctx
  = Mob
      { -- | UnitData of that mob
        _mobUnit :: UnitData,
        -- | Mob behaviour using some context
        _strategy :: ctx Action
      }

makeLenses ''LevellingStats

makeLenses ''Mob

makeLenses ''Player

makeLenses ''UnitData

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
createUnitData = UnitData False

-- | Something that can hit and run.
-- A typeclass for every active participant of a game. If it moves and participates in combat system, it is a unit.
class Unit u where
  -- | Returns 'UnitData' of a unit.
  asUnitData :: u -> UnitData

  -- | How unit is affected by 'UnitOp's.
  -- It is the main thing that differs a 'Unit' from 'UnitData'.
  applyUnitOp :: UnitOp a -> u -> (u, a)

-- | A version of applyUnitOp that discards the result
applyUnitOp_ :: Unit u => UnitOp a -> u -> u
applyUnitOp_ modifier u = fst $ applyUnitOp modifier u

instance Unit (AnyUnit ctx) where
  asUnitData (MkMob m) = asUnitData m
  asUnitData (MkPlayer p) = asUnitData p

  applyUnitOp modifier (MkMob m) = over _1 MkMob $ applyUnitOp modifier m
  applyUnitOp modifier (MkPlayer m) = over _1 MkPlayer $ applyUnitOp modifier m

instance Unit Player where
  asUnitData = _playerUnit

  applyUnitOp (Pure res) m = (m, res)
  applyUnitOp (Free (GetStats nextF)) m =
    applyUnitOp (nextF (Just $ m ^. playerUnit . stats)) m
  applyUnitOp (Free (ModifyStats f next)) u =
    applyUnitOp next (u & playerUnit . stats %~ f)
  applyUnitOp (Free (GetPosition nextF)) m =
    applyUnitOp (nextF (m ^. playerUnit . position)) m
  applyUnitOp (Free (ModifyPosition f next)) u =
    applyUnitOp next (u & playerUnit . position %~ f)
  applyUnitOp (Free (SetTimedUnitOp time modifier next)) u =
    applyUnitOp next $
      over (playerUnit . timedUnitOps) (addUnitOp time modifier) u
  applyUnitOp (Free (MoveTo coordTo next)) u =
    applyUnitOp next $ playerUnit . position .~ coordTo $ u
  applyUnitOp (Free (GetPortrait nextF)) u =
    applyUnitOp (nextF (u ^. playerUnit . portrait)) u
  applyUnitOp (Free (GetConfusion nextF)) u =
    applyUnitOp (nextF (u ^. playerUnit . confused)) u
  applyUnitOp (Free (ApplyEffect effect next)) u =
    applyUnitOp next $ applyEffect effect u
    where
      applyEffect (Damage dmg) = playerUnit . stats . health %~ subtract (fromNonNegative dmg)
      applyEffect (Heal h) = playerUnit . stats . health %~ (+) (fromNonNegative h)
      applyEffect (GiveExp _) = id

instance Unit (Mob ctx) where
  asUnitData = _mobUnit

  applyUnitOp (Pure res) m = (m, res)
  applyUnitOp (Free (GetStats nextF)) m =
    applyUnitOp (nextF (Just $ m ^. mobUnit . stats)) m
  applyUnitOp (Free (ModifyStats f next)) u =
    applyUnitOp next (u & mobUnit . stats %~ f)
  applyUnitOp (Free (GetPosition nextF)) m =
    applyUnitOp (nextF (m ^. mobUnit . position)) m
  applyUnitOp (Free (ModifyPosition f next)) u =
    applyUnitOp next (u & mobUnit . position %~ f)
  applyUnitOp (Free (SetTimedUnitOp time modifier next)) u =
    applyUnitOp next $ over (mobUnit . timedUnitOps) (addUnitOp time modifier) u
  applyUnitOp (Free (MoveTo coordTo next)) u =
    applyUnitOp next $ mobUnit . position .~ coordTo $ u
  applyUnitOp (Free (GetPortrait nextF)) u =
    applyUnitOp (nextF (u ^. mobUnit . portrait)) u
  applyUnitOp (Free (GetConfusion nextF)) u =
    applyUnitOp (nextF (u ^. mobUnit . confused)) u
  applyUnitOp (Free (ApplyEffect effect next)) u =
    applyUnitOp next $ applyEffect effect u
    where
      applyEffect (Damage dmg) = mobUnit . stats . health %~ subtract (fromNonNegative dmg)
      applyEffect (Heal h) = mobUnit . stats . health %~ (+) (fromNonNegative h)
      applyEffect (GiveExp _) = id

-- | Returns an active weapon unit data implies.
-- That is, returns equipped weapon or base weapon if none equipped
getWeapon :: UnitData -> WeaponItem
getWeapon unitData = fromMaybe (_baseWeapon unitData) (getEquippedWeapon $ _inventory unitData)

-- | Returns attack modifier this unit data implies
getAttackUnitOp :: UnitData -> EffectDesc
getAttackUnitOp unitData = getWeapon unitData ^. weaponAttackUnitOp

-- | Check whether a unit is alive
isAlive :: Unit u => u -> Bool
isAlive u = asUnitData u ^. stats . health > 0

makePlayer :: UnitData -> Player
makePlayer unitData = Player unitData (LevellingStats 0 0)

makeMob :: UnitData -> Mob ctx
makeMob unitData = Mob unitData undefined
