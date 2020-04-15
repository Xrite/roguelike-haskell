module Game.Modifiers.EffectDesc
  ( ModifierKey
  , EffectDesc (..)
  )
where

import Game.Modifiers.EffectAtom

type ModifierKey = String

data EffectDesc = Atom EffectAtom
                | DefaultModifier ModifierKey
                | Sequential [EffectDesc]
                -- TODO add timed modifier here? or to atom?