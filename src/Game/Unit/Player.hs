{-# LANGUAGE TemplateHaskell #-}
module Game.Unit.Player
  ( LevellingStats
  , Player
  )
where

import           Game.Effect
import           Control.Lens
import           Game.Unit.Unit
import           Control.Monad.Free
import           Game.Unit.TimedEffects
import           Game.Unit.Inventory            ( getAllWearableEffects
                                                , getWeaponEffect
                                                )


data LevellingStats = LevellingStats
  { _experience :: Int
  , _skillPoints :: Int
  }
makeLenses ''LevellingStats

data Player =
  Player { _playerUnit :: UnitData, _levelling :: LevellingStats}
makeLenses ''Player

instance Unit Player where
  asUnitData = _playerUnit

  applyEffect (Pure _) m = m
  applyEffect (Free (GetStats nextF)) m =
    applyEffect (nextF (Just $ m ^. playerUnit . stats)) m
  applyEffect (Free (SetStats newStats next)) u =
    applyEffect next (u & playerUnit . stats .~ newStats)
  applyEffect (Free (ModifyStats f next)) u =
    applyEffect next (u & playerUnit . stats %~ f)
  applyEffect (Free (SetTimedEffect time effect next)) u =
    applyEffect next
      $ over (playerUnit . timedEffects) (addEffect time effect) u
  applyEffect (Free (MoveTo coordTo next)) u =
    applyEffect next $ playerUnit . position .~ coordTo $ u

  attackEffect p =
    getWeaponEffect $ applyEffect wearableEff p ^. playerUnit . inventory
   where
    inv         = p ^. playerUnit . inventory
    wearableEff = getAllWearableEffects inv
