{-# LANGUAGE TemplateHaskell #-}

-- | Describes a 'Unit' type for mobs
module Game.Unit.Mob where

import           Control.Lens
import           Control.Monad.Free
import           Game.Modifiers.Modifier
import           Game.Unit.TimedModifiers
import           Game.Unit.Inventory            ( getAllWearableModifiers )
import           Game.Unit.Unit                 ( UnitData
                                                , Unit(..)
                                                , timedModifiers
                                                , stats
                                                , inventory
                                                , position
                                                )
import           Game.Modifiers.EffectAtom
import           Game.Unit.Stats                ( health )

-- | A mob is a simple computer-controlled 'Unit'.
data Mob = Mob {_unit :: UnitData}

makeLenses ''Mob

instance Unit Mob where
  asUnitData = _unit

  applyModifier (Pure _) m = m
  applyModifier (Free (GetStats nextF)) m =
    applyModifier (nextF (Just $ m ^. unit . stats)) m
  applyModifier (Free (SetStats newStats next)) u =
    applyModifier next (u & unit . stats .~ newStats)
  applyModifier (Free (ModifyStats f next)) u =
    applyModifier next (u & unit . stats %~ f)
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
