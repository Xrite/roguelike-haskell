{-# LANGUAGE DeriveFunctor #-}

module Game.Modifiers.EffectDesc
  ( UnitOpKey
  , EffectDescM
  , EffectDesc
  , EffectDescDSL(..)
  , effectAtom
  , effectTypical
  ) where

import Control.Monad.Free (Free(..))
import Game.Modifiers.EffectAtom

-- |A reference to a default 'UnitOp'
type UnitOpKey = String

-- TODO add timed modifier here? or to atom?
-- |DSL for describing effects.
data EffectDescDSL a
  = Atom EffectAtom a         -- ^ basic effect atom
  | TypicalUnitOp UnitOpKey a -- ^ default effect reference that will be substituted by 'UnitOpFactory' 
  deriving (Functor)

-- |A monad for describing effects
type EffectDescM a = Free EffectDescDSL a

-- |Usual parametrization of 'EffectDescM' 
type EffectDesc = EffectDescM ()

-- |Constructs an 'EffectDesc' for applying an 'EffectAtom'
effectAtom :: EffectAtom -> EffectDesc
effectAtom atom = Free $ Atom atom $ Pure ()

-- |Constructs an 'EffectDesc' for applying one of default 'UnitOp's
effectTypical :: UnitOpKey -> EffectDesc
effectTypical key = Free $ TypicalUnitOp key $ Pure ()
