{-# LANGUAGE TemplateHaskell #-}

-- | Describes a 'Unit' type for mobs
module Game.Unit.Mob where

import Control.Lens
import Control.Monad.Free
import Game.Modifiers.EffectAtom
import Game.Modifiers.UnitOp
import Game.Unit.Action
import Game.Unit.Stats (health)
import Game.Unit.TimedUnitOps
import Game.Unit.Unit

-- | A mob is a simple computer-controlled 'Unit'.
data Mob ctx
  = Mob
      { -- | UnitData of that mob
        _unit :: UnitData,
        -- | Mob behaviour
        _strategy :: ctx Action
      }

makeLenses ''Mob

instance Unit (Mob ctx) where
  asUnitData = _unit

  applyUnitOp (Pure res) m = (m, res)
  applyUnitOp (Free (GetStats nextF)) m =
    applyUnitOp (nextF (Just $ m ^. unit . stats)) m
  applyUnitOp (Free (ModifyStats f next)) u =
    applyUnitOp next (u & unit . stats %~ f)
  applyUnitOp (Free (GetPosition nextF)) m =
    applyUnitOp (nextF (m ^. unit . position)) m
  applyUnitOp (Free (ModifyPosition f next)) u =
    applyUnitOp next (u & unit . position %~ f)
  applyUnitOp (Free (SetTimedUnitOp time modifier next)) u =
    applyUnitOp next $ over (unit . timedUnitOps) (addUnitOp time modifier) u
  applyUnitOp (Free (MoveTo coordTo next)) u =
    applyUnitOp next $ unit . position .~ coordTo $ u
  applyUnitOp (Free (GetPortrait nextF)) u =
    applyUnitOp (nextF (u ^. unit . portrait)) u
  applyUnitOp (Free (ApplyEffect effect next)) u =
    applyUnitOp next $ applyEffect effect u
    where
      applyEffect (Damage dmg) = unit . stats . health %~ subtract dmg
      applyEffect (Heal h) = unit . stats . health %~ (+) h
      applyEffect (GiveExp _) = id
