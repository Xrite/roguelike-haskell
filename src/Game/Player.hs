{-# LANGUAGE TemplateHaskell #-}
module Game.Player where

import           Game.Stats
import           Game.Effect
import           Control.Lens
import           Game.TimedEffects
import           Control.Monad.Free
import           Game.Inventory
import           Game.Unit

data Player = Player { _experience :: Int, _unit :: UnitData }

makeLenses ''Player

instance Unit Player where
  asUnit p = _unit p

  applyEffect (Pure _) p = p
  applyEffect (Free (GetStats nextF)) p = applyEffect (nextF (p ^. unit . stats)) p
  applyEffect (Free (SetStats newStats next)) p =
    applyEffect next (p & unit . stats .~ newStats)
  applyEffect (Free (ModifyStats f next)) p =
    applyEffect next (p & unit . stats %~ f )
  applyEffect (Free (SetTimedEffect time effect next)) p = applyEffect next
    $ over (unit .timedEffects) (addEffect time effect) p
