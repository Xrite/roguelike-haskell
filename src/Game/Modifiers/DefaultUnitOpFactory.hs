module Game.Modifiers.DefaultUnitOpFactory where

import qualified Data.Map as Map (singleton)
import Game.Modifiers.UnitOpFactory
import Game.Modifiers.EffectDesc
import Game.Modifiers.UnitOp
import Game.Modifiers.EffectAtom (confuse)

defaultUnitOpFactory :: UnitOpFactory
defaultUnitOpFactory = makeUnitOpFactory $ Map.singleton "confuse" confusedEffect
  where
    confusedEffect = setTimedUnitOp 10 (const $ effectAtom confuse)
