module Game.Modifiers.ModifierFactory
  ( ModifierFactory
  , makeModifierFactory
  , buildModifier
  )
where

import Game.Modifiers.EffectDesc
import Game.Modifiers.Modifier
import Data.Map
import Control.Monad.Free (Free (..))

newtype ModifierFactory = ModifierFactory (Map ModifierKey (Modifier ()))

makeModifierFactory :: Map ModifierKey (Modifier ()) -> ModifierFactory
makeModifierFactory = ModifierFactory

buildModifier :: ModifierFactory -> EffectDesc -> Modifier ()
buildModifier factory (Free (Atom atom next)) = setEffect atom >> buildModifier factory next
buildModifier factory@(ModifierFactory mp) (Free (TypicalModifier key next)) = mp ! key >> buildModifier factory next
buildModifier _ (Pure ()) = Pure ()
