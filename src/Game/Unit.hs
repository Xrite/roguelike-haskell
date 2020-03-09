{-# LANGUAGE TemplateHaskell #-}

module Game.Unit where

import           Game.Stats
import           Game.Effect
import           Game.TimedEffects
import           Game.IO.GameIO
import           Game.GameLevels.GameLevel
import           Control.Monad.Free
import           Control.Lens

data Direction = Positive
               | Negative
               | Zero

data Action = Move Direction Direction

data Unit = Unit {
  _stats :: Stats, _timedEffects :: TimedEffects, _control :: GameLevel -> [Unit] -> GameIO Action }
makeLenses ''Unit

makeMob :: Stats -> (GameLevel -> [Unit] -> Action) -> Unit
makeMob stat ai = Unit stat (TimedEffects []) $ \ lvl units -> pureGameIO (ai lvl units)

applyEffect :: Effect () -> Unit -> Unit
applyEffect (Pure _) unit = unit
applyEffect (Free (GetStats nextF)) unit = applyEffect (nextF (_stats unit)) unit
applyEffect (Free (SetStats newStats next)) unit =
  applyEffect next (unit { _stats = newStats })
applyEffect (Free (ModifyStats f next)) unit =
  applyEffect next (unit { _stats = f (_stats unit) })
applyEffect (Free (SetTimedEffect time effect next)) unit = applyEffect next $ over timedEffects (addEffect time effect) unit
