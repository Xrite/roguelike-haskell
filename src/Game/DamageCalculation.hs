module Game.DamageCalculation where

import           Prelude                 hiding ( head )
import           Game.Unit.Inventory
import           Game.Unit.Unit
import           Game.Item

{- attack :: (Unit a, Unit b) => a -> b -> (a, b)
attack attacker victim =
    let attackerUnit = asUnit atacker
        victimUnit   = asUnit victim
    in (attacker, applyEffect (createAttackEffect attacker) victim) -}

