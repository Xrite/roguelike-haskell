{-# LANGUAGE TemplateHaskell #-}

-- | Describes a 'Unit' type for players 
module Game.Unit.Player (LevellingStats, Player, makePlayer) where

import           Game.Effects.Modifier
import           Control.Lens
import           Game.Unit.Unit
import           Control.Monad.Free
import           Game.Unit.TimedEffects
import           Game.Unit.Inventory (getAllWearableEffects)
import Game.Effects.EffectAtom
import Game.Unit.Stats (health)

-- | Describes everything regarding level-up system of a 'Player'
data LevellingStats =
  LevellingStats { _experience :: Int, _skillPoints :: Int }

makeLenses ''LevellingStats

-- | A unit that can get experience points and level-ups. Controlled from the outside world.
data Player = Player { _playerUnit :: UnitData, _levelling :: LevellingStats }

makeLenses ''Player

makePlayer :: UnitData -> Player
makePlayer unitData = Player unitData (LevellingStats 0 0)

instance Unit Player where
  asUnitData = _playerUnit

  applyEffect (Pure _) m = m
  applyEffect (Free (GetStats nextF)) m =
    applyEffect (nextF (Just $ m ^. playerUnit . stats)) m
  applyEffect (Free (SetStats newStats next)) u =
    applyEffect next (u & playerUnit . stats .~ newStats)
  applyEffect (Free (ModifyStats f next)) u =
    applyEffect next (u & playerUnit . stats %~ f)
  applyEffect (Free (SetTimedEffect time effect next)) u = applyEffect next
    $ over (playerUnit . timedEffects) (addEffect time effect) u
  applyEffect (Free (MoveTo coordTo next)) u =
    applyEffect next $ playerUnit . position .~ coordTo $ u
  applyEffect (Free (ApplyEffect effect next)) u =
    applyEffect next $ appEffect effect u
    where
      appEffect (Damage dmg) = playerUnit . stats . health %~ subtract dmg
      appEffect (Heal h) = playerUnit . stats . health %~ (+) h
      appEffect (GiveExp _) = id
