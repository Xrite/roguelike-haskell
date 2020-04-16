module Game.Unit.DamageCalculation where

import           Prelude                 hiding ( head )
import qualified Game.Unit.Unit as Unit
import qualified Game.Scenario as Scenario

attack :: (Unit.Unit a, Unit.Unit b) => a -> b -> Scenario.Scenario unitAccessor ()
attack attacker attacked = do
  return ()
  where
    attackerInv = Unit.inventory $ Unit.asUnitData attacker 

    attackedInv = Unit.inventory $ Unit.asUnitData attacked
