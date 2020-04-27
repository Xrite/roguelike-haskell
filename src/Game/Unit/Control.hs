{-# LANGUAGE DeriveGeneric #-}

module Game.Unit.Control (TaggedControl (..)) where

import GHC.Generics (Generic)

data TaggedControl = Aggressive | Passive (Int, Int) | Avoiding | DoNothing deriving (Generic)