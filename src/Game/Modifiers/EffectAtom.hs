module Game.Modifiers.EffectAtom
  ( EffectAtom (..)
  , NonNegative
  , fromNonNegative
  , damage
  , heal
  , giveExp
  )
where

data EffectAtom = Damage NonNegative
                | Heal NonNegative
                | GiveExp NonNegative

newtype NonNegative = NonNegative Int

nonNegative :: Int -> NonNegative
nonNegative n =
  if n >= 0
    then NonNegative n
    else error "Negative int in non-negative context"

fromNonNegative :: NonNegative -> Int
fromNonNegative (NonNegative n) = n

damage = Damage . nonNegative
heal = Heal . nonNegative
giveExp = GiveExp . nonNegative