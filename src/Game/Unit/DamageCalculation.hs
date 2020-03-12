module Game.Unit.DamageCalculation where

import           Prelude                 hiding ( head )
import           Game.Unit.Unit

attack :: (Unit a, Unit b) => a -> b -> (a, b)
attack attacker victim =
    (attacker, applyEffect (attackEffect attacker) victim)

