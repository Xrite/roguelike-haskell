{-# LANGUAGE DeriveFunctor #-}

module Game.Modifiers.EffectDesc
  ( UnitOpKey
  , EffectDescM
  , EffectDesc
  , EffectDescDSL (..)
  , effectAtom
  , effectTypical
  )
where

import Game.Modifiers.EffectAtom
import Control.Monad.Free (Free (..))

type UnitOpKey = String

-- TODO add timed modifier here? or to atom?
data EffectDescDSL a = Atom EffectAtom a
                | TypicalUnitOp UnitOpKey a
                deriving (Functor)

type EffectDescM a = Free EffectDescDSL a
type EffectDesc = EffectDescM ()

effectAtom :: EffectAtom -> EffectDesc
effectAtom atom = Free $ Atom atom $ Pure ()

effectTypical :: UnitOpKey -> EffectDesc
effectTypical key = Free $ TypicalUnitOp key $ Pure ()