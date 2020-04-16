module Game.Unit.DamageCalculation where

import Control.Lens
import Game.Modifiers.Modifier
import Game.Modifiers.ModifierFactory (ModifierFactory, buildModifier)
import Game.Unit.Inventory
import Game.Unit.Unit
import Prelude hiding (head)

attack :: (Unit a, Unit b) => ModifierFactory -> a -> b -> (a, b)
attack fact attacker victim =
  (attacker, newVictim)
  where
    (newVictim, _) = applyModifier (attackModifier fact attacker) victim

attackModifier :: Unit u => ModifierFactory -> u -> Modifier ()
attackModifier factory u = buildModifier factory u $ getAttackModifier . asUnitData $ applyModifier_ wearableModifier u
  where
    inv = asUnitData u ^. inventory
    wearableEffect = getAllWearableModifiers inv
    wearableModifier = buildModifier factory u wearableEffect
