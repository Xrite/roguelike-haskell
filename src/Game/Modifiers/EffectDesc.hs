{-# LANGUAGE DeriveFunctor #-}

module Game.Modifiers.EffectDesc
  ( ModifierKey
  , EffectDescM
  , EffectDesc
  , EffectDescDSL (..)
  , effectAtom
  , effectTypical
  )
where

import Game.Modifiers.EffectAtom
import Control.Monad.Free (Free (..))

type ModifierKey = String

-- TODO add timed modifier here? or to atom?
data EffectDescDSL a = Atom EffectAtom a
                | TypicalModifier ModifierKey a
                deriving (Functor)

type EffectDescM a = Free EffectDescDSL a
type EffectDesc = EffectDescM ()

effectAtom :: EffectAtom -> EffectDesc
effectAtom atom = Free $ Atom atom $ Pure ()

effectTypical :: ModifierKey -> EffectDesc
effectTypical key = Free $ TypicalModifier key $ Pure ()