{-# LANGUAGE TemplateHaskell #-}

-- | Describes a 'Unit' type for mobs
module Game.Unit.Mob where

import Control.Lens
import Control.Monad.Free
import Game.Modifiers.EffectAtom
import Game.Modifiers.Modifier
import Game.Unit.Action
import Game.Unit.Inventory (getAllWearableModifiers)
import Game.Unit.Stats (health)
import Game.Unit.TimedModifiers
import Game.Unit.Unit
  ( Unit (..),
    UnitData,
    inventory,
    position,
    stats,
    timedModifiers,
  )

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

  applyModifier (Pure res) m = (m, res)
  applyModifier (Free (GetStats nextF)) m =
    applyModifier (nextF (Just $ m ^. unit . stats)) m
  applyModifier (Free (ModifyStats f next)) u =
    applyModifier next (u & unit . stats %~ f)
  applyModifier (Free (GetPosition nextF)) m =
    applyModifier (nextF (m ^. unit . position)) m
  applyModifier (Free (ModifyPosition f next)) u =
    applyModifier next (u & unit . position %~ f)
  applyModifier (Free (SetTimedModifier time modifier next)) u =
    applyModifier next $ over (unit . timedModifiers) (addModifier time modifier) u
  applyModifier (Free (MoveTo coordTo next)) u =
    applyModifier next $ unit . position .~ coordTo $ u
  applyModifier (Free (ApplyEffect effect next)) u =
    applyModifier next $ applyEffect effect u
    where
      applyEffect (Damage dmg) = unit . stats . health %~ subtract dmg
      applyEffect (Heal h) = unit . stats . health %~ (+) h
      applyEffect (GiveExp _) = id
