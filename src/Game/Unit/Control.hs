module Game.Unit.Control where

data TaggedControl = Aggressive | Passive (Int, Int) | Avoiding | DoNothing