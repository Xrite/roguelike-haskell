{-# LANGUAGE TemplateHaskell #-}
module Game.Mob where

import           Game.Effect
import           Game.TimedEffects
import           Control.Monad.Free
import           Control.Lens
import           Game.Unit

data Mob = Mob { _unit :: UnitData }

makeLenses ''Mob

instance Unit Mob where
  asUnit = _unit

  applyEffect (Pure _) m = m
  applyEffect (Free (GetStats nextF)) m = applyEffect (nextF (m ^. unit . stats)) m
  applyEffect (Free (SetStats newStats next)) u =
    applyEffect next (u & unit . stats .~ newStats)
  applyEffect (Free (ModifyStats f next)) u =
    applyEffect next (u & unit . stats %~ f)
  applyEffect (Free (SetTimedEffect time effect next)) u = applyEffect next
    $ over (unit . timedEffects) (addEffect time effect) u
