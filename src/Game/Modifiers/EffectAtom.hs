{-# LANGUAGE DeriveGeneric #-}

module Game.Modifiers.EffectAtom
  ( EffectAtom (..)
  , NonNegative
  , fromNonNegative
  , damage
  , heal
  , giveExp
  , confuse
  )
where

import GHC.Generics (Generic)


-- |Describes basic effects that might be applied to a unit as part of game mechanics.
data EffectAtom = Damage NonNegative -- ^ Deal damage to a unit
                | Heal NonNegative -- ^ Heal a unit
                | GiveExp NonNegative -- ^ Give experience points to a unit
                | SetConfusion Bool -- ^ Confuses or unconfuses unit
                deriving (Generic, Eq)

-- |A non negative Int.
newtype NonNegative = NonNegative Int deriving (Generic, Eq)

-- |Constructs a 'NonNegative' if provided value is not negative or throws an error otherwise.
nonNegative :: Int -> NonNegative
nonNegative n =
  if n >= 0
    then NonNegative n
    else error "Negative int in non-negative context"

-- |Extracts value from 'NonNegative'
fromNonNegative :: NonNegative -> Int
fromNonNegative (NonNegative n) = n

-- |Constructs a damaging 'EffectAtom'
damage :: Int -> EffectAtom
damage = Damage . nonNegative

-- |Constructs a healing 'EffectAtom'
heal :: Int -> EffectAtom
heal = Heal . nonNegative

-- |Constructs an experience giving 'EffectAtom'
giveExp :: Int -> EffectAtom
giveExp = GiveExp . nonNegative

confuse :: EffectAtom
confuse = SetConfusion True