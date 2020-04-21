{-# LANGUAGE KindSignatures #-}
module Game.Unit.Unit
  ( UnitData,
    Unit (..),
    Mob,
    Player,
    AnyUnit (..),
    isAlive,
    applyUnitOp_,
  )
where

import Control.Lens
import Control.Monad.Free
import Game.Modifiers.EffectAtom
import Game.Modifiers.EffectDesc (EffectDesc)
import Game.Modifiers.UnitOp
import Game.Unit.Action
import Game.Unit.Stats
import Game.Unit.TimedUnitOps

data UnitData

data AnyUnit = MkMob Mob | MkPlayer Player

data LevellingStats

data Player = Player {_playerUnit :: UnitData, _levelling :: LevellingStats}

data Mob


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

-- | Check whether a unit is alive
isAlive :: Unit u => u -> Bool

makePlayer :: UnitData -> Player

instance Unit AnyUnit

instance Unit Player

instance Unit Mob