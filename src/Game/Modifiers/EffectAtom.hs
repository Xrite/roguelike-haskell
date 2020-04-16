module Game.Modifiers.EffectAtom
  ( EffectAtom (..)
  )
where

data EffectAtom = Damage Int
                | Heal Int
                | GiveExp Int
