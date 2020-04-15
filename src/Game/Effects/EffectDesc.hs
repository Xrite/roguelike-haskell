module Game.Effects.EffectDesc
  ( EffectKey
  , EffectDesc
  )
where

import Game.Effects.EffectAtom

type EffectKey = String

data EffectDesc = Atom EffectAtom
                | DefaultEffect EffectKey
                | Sequential [EffectDesc]
                -- TODO add timed effect here? or to atom?