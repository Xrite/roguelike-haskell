{-# LANGUAGE FlexibleInstances #-}

module Game.Modifiers.EffectSerialization () where

import Data.Binary (Binary)
import Game.Modifiers.EffectAtom (EffectAtom, NonNegative)
import Game.Modifiers.EffectDesc (EffectDescDSL)
import Control.Monad.Free (Free)

instance Binary NonNegative
instance Binary EffectAtom

instance Binary a => Binary (EffectDescDSL a)
instance Binary a => Binary (Free EffectDescDSL a)