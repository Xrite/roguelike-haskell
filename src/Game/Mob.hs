{-# LANGUAGE TemplateHaskell #-}

module Game.Mob where

import           Game.Stats
import           Game.Effect
import           Game.TimedEffects
import           Control.Monad.Free
import           Control.Lens

data Mob = Mob { _stats :: Stats, _timedEffects :: TimedEffects }

makeLenses ''Mob

applyEffect :: Effect () -> Mob -> Mob
applyEffect (Pure _) mob = mob
applyEffect (Free (GetStats nextF)) mob = applyEffect (nextF (_stats mob)) mob
applyEffect (Free (SetStats newStats next)) mob =
  applyEffect next (mob { _stats = newStats })
applyEffect (Free (ModifyStats f next)) mob =
  applyEffect next (mob { _stats = f (_stats mob) })
applyEffect (Free (SetTimedEffect time effect next)) mob = applyEffect next $ over timedEffects (addEffect time effect) mob
