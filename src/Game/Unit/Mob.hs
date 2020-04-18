{-# LANGUAGE TemplateHaskell #-}

-- | Describes a 'Unit' type for mobs
module Game.Unit.Mob where

import           Control.Lens
import           Control.Monad.Free
import           Game.Modifiers.UnitOp
import           Game.Unit.TimedUnitOps
import           Game.Unit.Inventory            ( getAllWearableUnitOps )
import           Game.Unit.Unit                 ( UnitData
                                                , Unit(..)
                                                , timedUnitOps
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

  applyUnitOp (Pure _) m = m
  applyUnitOp (Free (GetStats nextF)) m =
    applyUnitOp (nextF (Just $ m ^. unit . stats)) m
  applyUnitOp (Free (SetStats newStats next)) u =
    applyUnitOp next (u & unit . stats .~ newStats)
  applyUnitOp (Free (ModifyStats f next)) u =
    applyUnitOp next (u & unit . stats %~ f)
  applyUnitOp (Free (SetTimedUnitOp time modifier next)) u =
    applyUnitOp next $ over (unit . timedUnitOps) (addUnitOp time modifier) u
  applyUnitOp (Free (MoveTo coordTo next)) u =
    applyUnitOp next $ unit . position .~ coordTo $ u
  applyUnitOp (Free (ApplyEffect effect next)) u =
    applyUnitOp next $ applyEffect effect u
    where
      applyEffect (Damage dmg) = unit . stats . health %~ subtract dmg
      applyEffect (Heal h) = unit . stats . health %~ (+) h
      applyEffect (GiveExp _) = id
