{-# LANGUAGE DeriveGeneric #-}
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
import Data.Binary (Binary)
import GHC.Generics (Generic)
import Data.Functor.Classes (Eq1 (liftEq), eq1, Show1)

-- |A reference to a default 'UnitOp'
type UnitOpKey = String

-- TODO add timed modifier here? or to atom?
-- |DSL for describing effects.
data EffectDescDSL a
  = Atom EffectAtom a         -- ^ basic effect atom
  | TypicalUnitOp UnitOpKey a -- ^ default effect reference that will be substituted by 'UnitOpFactory' 
  deriving (Functor, Generic, Eq)

instance Eq1 EffectDescDSL where
  liftEq eq a b = retrieve a `eq` retrieve b 
    where
      retrieve (Atom _ v) = v
      retrieve (TypicalUnitOp _ v) = v

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
