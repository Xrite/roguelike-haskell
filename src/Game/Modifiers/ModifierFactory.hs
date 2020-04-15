module Game.Modifiers.ModifierFactory
  ( ModifierFactory
  , makeModifierFactory
  , buildModifier
  )
where

import Game.Modifiers.EffectDesc
import Game.Modifiers.Modifier
import Data.Map
import Data.Foldable (foldrM)

newtype ModifierFactory = ModifierFactory (Map ModifierKey (Modifier ()))

makeModifierFactory :: Map ModifierKey (Modifier ()) -> ModifierFactory
makeModifierFactory = ModifierFactory

buildModifier :: ModifierFactory -> EffectDesc -> Modifier ()
buildModifier _ (Atom atom) = setEffect atom
buildModifier factory (Sequential seq) = foldrM (const . buildModifier factory) () seq
buildModifier (ModifierFactory mp) (DefaultModifier key) = mp ! key
