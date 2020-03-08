{-# LANGUAGE TemplateHaskell #-}

module Game.Player where

import           Game.Stats
import           Game.Effect
import           Control.Lens
import           Game.TimedEffects
import           Control.Monad.Free

data Player =
  Player { _experience :: Int, _stats :: Stats, _timedEffects :: TimedEffects }

makeLenses ''Player

applyEffect :: Effect () -> Player -> Player
applyEffect (Pure _) p = p
applyEffect (Free (GetStats nextF)) p = applyEffect (nextF (_stats p)) p
applyEffect (Free (SetStats newStats next)) p =
  applyEffect next (p { _stats = newStats })
applyEffect (Free (ModifyStats f next)) p =
  applyEffect next (p { _stats = f (_stats p) })
applyEffect (Free (SetTimedEffect time effect next)) p = applyEffect next
  $ over timedEffects (addEffect time effect) p
