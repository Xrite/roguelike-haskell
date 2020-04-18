module Game.Unit.DamageCalculation where

import           Prelude                 hiding ( head )
import           Game.Unit.Unit
import Game.Modifiers.UnitOpFactory (UnitOpFactory)

attack :: (Unit a, Unit b) => UnitOpFactory -> a -> b -> (a, b)
attack fact attacker victim =
    (attacker, applyUnitOp (attackUnitOp fact attacker) victim)

