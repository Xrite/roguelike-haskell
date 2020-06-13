{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Describes common interface for all units in the game.
module Game.Unit.Unit where

{- ( UnitData (..),
  confused,
  position,
  stats,
  timedUnitOps,
  inventory,
  baseWeapon,
  portrait,
  createUnitData,
  LevellingStats,
  experience,
  skillPoints,
  Player,
  playerUnitData,
  playerLevelling,
  Mob,
  mobUnitData,
  mobControlTag,
  Unit (..),
  getWeapon,
  getAttackUnitOp,
  isAlive,
  makeDefaultPlayer,
  makeDefaultMob,
  unitWithModifiers,
) -}

import Control.Lens
import Control.Monad.Free
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Game.Item
import Game.Modifiers.EffectAtom
import Game.Modifiers.EffectDesc (EffectDesc)
import Game.Modifiers.UnitOp
import Game.Modifiers.UnitOpFactory (UnitOpFactory, buildUnitOp)
import Game.Position (Position)
import Game.Unit.Control
import Game.Unit.Inventory
import Game.Unit.Stats
import Game.Unit.TimedUnitOps

-- | Common data of all units.
data UnitData
  = UnitData
      { -- | If unit is confused
        _confused :: Bool,
        -- | Position in game
        _position :: Position,
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
  deriving (Generic)

-- | Tagged union of units
data Unit
  = MkMob Mob
  | MkPlayer Player
  deriving (Generic)

data LevellingStats
  = LevellingStats
      { _experience :: Int,
        _skillPoints :: Int
      }
  deriving (Generic)

-- | A unit that can get experience points and level-ups. Controlled from the outside world.
data Player
  = Player
      { _playerUnitData :: UnitData,
        _playerLevelling :: LevellingStats,
        -- | How a player should apply UnitOp
        --_playerApplyUnitOp :: forall a. UnitOp a -> Player -> (Player, a)
        _playerApplyUnitOpTag :: TaggedPlayerApplyUnitOp
      }
  deriving (Generic)

-- | A mob is a simple computer-controlled 'Unit'.
data Mob
  = Mob
      { -- | UnitData of that mob
        _mobUnitData :: UnitData,
        -- | Mob behavior using some context
        _mobControlTag :: TaggedControl,
        -- | How a mob should apply UnitOp
        --_mobApplyUnitOp :: forall a. UnitOp a -> Mob -> (Mob, a)
        _mobApplyUnitOpTag :: TaggedMobApplyUnitOp
      }
  deriving (Generic)

data TaggedPlayerApplyUnitOp = DefaultPlayerApplyUnitOp
  deriving (Generic)

data TaggedMobApplyUnitOp = DefaultMobApplyUnitOp
  deriving (Generic)

makeLenses ''LevellingStats

makeLenses ''Mob

makeLenses ''Player

makeLenses ''UnitData

-- | Constructs a new 'UnitData'.
createUnitData ::
  -- | Initial position
  Position ->
  -- | Stats of a unit
  Stats ->
  -- | Timed modifiers that are affecting the unit
  TimedUnitOps ->
  -- | Inventory on a unit
  Inventory ->
  -- | A weapon to use when unit is fighting bare-hand
  WeaponItem ->
  -- | How to display this unit
  Char ->
  -- | Constructed 'Unit'
  UnitData
createUnitData = UnitData False

applyUnitOp :: Unit -> UnitOp a -> (Unit, a)
applyUnitOp (MkPlayer p) op = over _1 MkPlayer $ (dispatchPlayerApplyUnitOp $ p ^. playerApplyUnitOpTag) op p
applyUnitOp (MkMob m) op = over _1 MkMob $ (dispatchMobApplyUnitOp $ m ^. mobApplyUnitOpTag) op m

applyUnitOp_ :: Unit -> UnitOp a -> Unit
applyUnitOp_ (MkPlayer p) op = MkPlayer . fst $ (dispatchPlayerApplyUnitOp $ p ^. playerApplyUnitOpTag) op p
applyUnitOp_ (MkMob m) op = MkMob . fst $ (dispatchMobApplyUnitOp $ m ^. mobApplyUnitOpTag) op m

getUnitData :: Unit -> UnitData
getUnitData (MkPlayer p) = p ^. playerUnitData
getUnitData (MkMob m) = m ^. mobUnitData

-- | A version of applyUnitOp that discards the result
-- applyUnitOp_ :: UnitOp a -> Unit -> Unit
-- applyUnitOp_ modifier u = fst $ applyUnitOp modifier u
defaultPlayerApplyUnitOp op p = case op of
  Pure res -> (p, res)
  Free (GetStats nextF) -> defaultPlayerApplyUnitOp (nextF (Just $ p ^. playerUnitData . stats)) p
  Free (ModifyStats f next) -> defaultPlayerApplyUnitOp next (p & playerUnitData . stats %~ f)
  Free (GetPosition nextF) -> defaultPlayerApplyUnitOp (nextF (p ^. playerUnitData . position)) p
  Free (ModifyPosition f next) -> defaultPlayerApplyUnitOp next (p & playerUnitData . position %~ f)
  Free (SetTimedUnitOp time modifier next) ->
    defaultPlayerApplyUnitOp next $
      over (playerUnitData . timedUnitOps) (addUnitOp time modifier) p
  Free (MoveTo coordTo next) -> defaultPlayerApplyUnitOp next $ playerUnitData . position .~ coordTo $ p
  Free (GetPortrait nextF) -> defaultPlayerApplyUnitOp (nextF (p ^. playerUnitData . portrait)) p
  (Free (GetConfusion nextF)) -> defaultPlayerApplyUnitOp (nextF (p ^. playerUnitData . confused)) p
  (Free (TickTimedEffects nextF)) -> defaultPlayerApplyUnitOp nextF $ playerUnitData . timedUnitOps %~ tick $ p
  (Free (ApplyEffect effect next)) -> defaultPlayerApplyUnitOp next $ applyEffect effect p
  where
    applyEffect (Damage dmg) = playerUnitData . stats . health %~ subtract (fromNonNegative dmg)
    applyEffect (Heal h) = playerUnitData . stats . health %~ (+) (fromNonNegative h)
    applyEffect (GiveExp _) = id
    applyEffect (SetConfusion c) = playerUnitData . confused .~ c

defaultMobApplyUnitOp ::
  UnitOp a ->
  Mob ->
  (Mob, a)
defaultMobApplyUnitOp op m = case op of
  (Pure res) -> (m, res)
  (Free (GetStats nextF)) ->
    defaultMobApplyUnitOp (nextF (Just $ m ^. mobUnitData . stats)) m
  (Free (ModifyStats f next)) ->
    defaultMobApplyUnitOp next (m & mobUnitData . stats %~ f)
  (Free (GetPosition nextF)) ->
    defaultMobApplyUnitOp (nextF (m ^. mobUnitData . position)) m
  (Free (ModifyPosition f next)) ->
    defaultMobApplyUnitOp next (m & mobUnitData . position %~ f)
  (Free (SetTimedUnitOp time modifier next)) ->
    defaultMobApplyUnitOp next $ over (mobUnitData . timedUnitOps) (addUnitOp time modifier) m
  (Free (MoveTo coordTo next)) ->
    defaultMobApplyUnitOp next $ mobUnitData . position .~ coordTo $ m
  (Free (GetPortrait nextF)) ->
    defaultMobApplyUnitOp (nextF (m ^. mobUnitData . portrait)) m
  (Free (GetConfusion nextF)) ->
    defaultMobApplyUnitOp (nextF (m ^. mobUnitData . confused)) m
  (Free (TickTimedEffects nextF)) ->
    defaultMobApplyUnitOp nextF $ mobUnitData . timedUnitOps %~ tick $ m
  (Free (ApplyEffect effect next)) ->
    defaultMobApplyUnitOp next $ applyEffect effect m
    where
      applyEffect (Damage dmg) = mobUnitData . stats . health %~ subtract (fromNonNegative dmg)
      applyEffect (Heal h) = mobUnitData . stats . health %~ (+) (fromNonNegative h)
      applyEffect (GiveExp _) = id
      applyEffect (SetConfusion c) = mobUnitData . confused .~ c

-- | Returns an active weapon unit data implies.
-- That is, returns equipped weapon or base weapon if none equipped
getWeapon :: UnitData -> WeaponItem
getWeapon unitData = fromMaybe (_baseWeapon unitData) (getEquippedWeapon $ _inventory unitData)

-- | Returns attack modifier this unit data implies
getAttackUnitOp :: UnitData -> EffectDesc
getAttackUnitOp unitData = getWeapon unitData ^. weaponAttackUnitOp

-- | Check whether a unit is alive
isAlive :: Unit -> Bool
isAlive u = getUnitData u ^. stats . health > 0

-- | Check whether a player is alive
isPlayerAlive :: Player -> Bool
isPlayerAlive p = p ^. playerUnitData . stats . health > 0

-- | Check whether a mob is alive
isMobAlive :: Mob -> Bool
isMobAlive m = m ^. mobUnitData . stats . health > 0

dispatchPlayerApplyUnitOp DefaultPlayerApplyUnitOp = defaultPlayerApplyUnitOp

dispatchMobApplyUnitOp DefaultMobApplyUnitOp = defaultMobApplyUnitOp

makeDefaultPlayer :: UnitData -> Player
makeDefaultPlayer unitData =
  Player
    { _playerUnitData = unitData,
      _playerLevelling = defaultLevelling,
      _playerApplyUnitOpTag = DefaultPlayerApplyUnitOp
    }

defaultLevelling =
  LevellingStats
    { _experience = 0,
      _skillPoints = 0
    }

makeDefaultMob :: UnitData -> TaggedControl -> Mob
makeDefaultMob unitData tag =
  Mob
    { _mobUnitData = unitData,
      _mobControlTag = tag,
      _mobApplyUnitOpTag = DefaultMobApplyUnitOp
    }

-- | Applies all modifiers from wearables and timed effects to a unit
unitWithModifiers :: UnitOpFactory -> Unit -> Unit
unitWithModifiers factory u = applyUnitOp_ u allUnitOps
  where
    inv = getUnitData u ^. inventory
    wearableEffect = getAllWearableUnitOps inv
    wearableUnitOp = buildUnitOp factory wearableEffect
    timedEffectsOp = composeUnitOp $ _timedUnitOps $ getUnitData u
    allUnitOps = wearableUnitOp >>= const (buildUnitOp factory timedEffectsOp)
