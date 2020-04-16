module Game.Modifiers.ModifierFactory
  ( ModifierFactory
  , makeModifierFactory
  , buildModifier
  )
where

import Game.Modifiers.EffectDesc
import Game.Modifiers.Modifier
import Data.Map
import Game.Unit.Unit
import Control.Monad.Free (Free (..))

newtype ModifierFactory = ModifierFactory (Map ModifierKey (Modifier ()))

makeModifierFactory :: Map ModifierKey (Modifier ()) -> ModifierFactory
makeModifierFactory = ModifierFactory

buildModifier :: Unit u => ModifierFactory -> u -> EffectDesc -> Modifier ()
buildModifier factory u (Free (Atom atom next)) = setEffect atom >> buildModifier factory u next
buildModifier factory@(ModifierFactory mp) u (Free (TypicalModifier key next)) = mp ! key >> buildModifier factory u next
buildModifier _ u (Pure ()) = Pure ()
