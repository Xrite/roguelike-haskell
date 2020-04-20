module Game.Unit.DamageCalculation where

import Control.Lens
import Game.Modifiers.UnitOp
import Game.Modifiers.UnitOpFactory (UnitOpFactory, buildUnitOp)
import Game.Unit.Inventory
import Game.Unit.Unit
import Prelude hiding (head)

attack :: (Unit a, Unit b) => UnitOpFactory -> a -> b -> (UnitOp (), UnitOp ())
attack fact attacker victim =
  (return (), attackUnitOp fact attacker)

attackUnitOp :: Unit u => UnitOpFactory -> u -> UnitOp ()
attackUnitOp factory u = buildUnitOp factory $ getAttackUnitOp . asUnitData $ applyUnitOp_ wearableUnitOp u
  where
    inv = asUnitData u ^. inventory
    wearableEffect = getAllWearableUnitOps inv
    wearableUnitOp = buildUnitOp factory wearableEffect
