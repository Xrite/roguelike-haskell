{-# LANGUAGE TemplateHaskell #-}
module Game.Unit.Player where

import           Game.Effect
import           Control.Lens
import           Game.Unit.Unit
import           Control.Monad.Free
import           Game.Unit.TimedEffects


data LevellingStats = LevellingStats
  { _experience :: Int
  , _skillPoints :: Int
  }
makeLenses ''LevellingStats

data Player =
  Player {
  -- |Corresponding Unit
  _playerUnit :: UnitData
  , _levelling :: LevellingStats
  }
makeLenses ''Player

instance Unit Player where
  asUnitData p = _playerUnit p

  applyEffect (Pure _) m = m
  applyEffect (Free (GetStats nextF)) m =
    applyEffect (nextF (m ^. playerUnit . stats)) m
  applyEffect (Free (SetStats newStats next)) u =
    applyEffect next (u & playerUnit . stats .~ newStats)
  applyEffect (Free (ModifyStats f next)) u =
    applyEffect next (u & playerUnit . stats %~ f)
  applyEffect (Free (SetTimedEffect time effect next)) u =
    applyEffect next
      $ over (playerUnit . timedEffects) (addEffect time effect) u
