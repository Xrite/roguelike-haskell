module Game.Unit.DamageCalculation where

import Game.Modifiers.UnitOp
import Game.Modifiers.UnitOpFactory (UnitOpFactory, buildUnitOp)
import Game.Unit.Unit
import Prelude hiding (head)

-- | Calculates units after attack
attack :: UnitOpFactory -> AnyUnit -> AnyUnit -> (AnyUnit, AnyUnit)
attack fact attacker attacked =
  (attacker, applyUnitOp_ (attackUnitOp fact attacker) attacked)

attackUnitOp :: Unit u => UnitOpFactory -> u -> UnitOp ()
attackUnitOp factory u = buildUnitOp factory $ getAttackUnitOp . asUnitData $ unitWithModifiers factory u
