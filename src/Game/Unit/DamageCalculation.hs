module Game.Unit.DamageCalculation where

import Control.Lens
import Game.Modifiers.UnitOp
import Game.Modifiers.UnitOpFactory (UnitOpFactory, buildUnitOp)
import Game.Unit.Inventory
import Game.Unit.Unit
import Prelude hiding (head)

-- | Calculates units after attack
attack :: UnitOpFactory -> AnyUnit ctx -> AnyUnit ctx -> (AnyUnit ctx, AnyUnit ctx)
attack fact attacker attacked =
  (attacker, applyUnitOp_ (attackUnitOp fact attacker) attacked)

attackUnitOp :: Unit u => UnitOpFactory -> u -> UnitOp ()
attackUnitOp factory u = buildUnitOp factory $ getAttackUnitOp . asUnitData $ applyUnitOp_ wearableUnitOp u
  where
    inv = asUnitData u ^. inventory
    wearableEffect = getAllWearableUnitOps inv
    wearableUnitOp = buildUnitOp factory wearableEffect
