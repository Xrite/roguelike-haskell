module Game.Unit.DamageCalculation where

import Game.Modifiers.UnitOp
import Game.Modifiers.UnitOpFactory (UnitOpFactory, buildUnitOp)
import Game.Unit.Unit
import Prelude hiding (head)

-- | Calculates units after attack
attack :: UnitOpFactory -> Unit -> Unit -> (Unit, Unit)
attack fact attacker attacked =
  (attacker, applyUnitOp_ attacked (attackUnitOp fact attacker))

attackUnitOp :: UnitOpFactory -> Unit -> UnitOp ()
attackUnitOp factory u = buildUnitOp factory $ getAttackUnitOp . getUnitData $ unitWithModifiers factory u
