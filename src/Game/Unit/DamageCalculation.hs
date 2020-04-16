module Game.Unit.DamageCalculation where

import           Prelude                 hiding ( head )
import           Game.Unit.Unit
import Game.Modifiers.ModifierFactory (ModifierFactory)

attack :: (Unit a, Unit b) => ModifierFactory -> a -> b -> (a, b)
attack fact attacker victim =
    (attacker, applyModifier (attackModifier fact attacker) victim)

