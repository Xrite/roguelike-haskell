{-# LANGUAGE TemplateHaskell #-}

-- | Describes a 'Unit' type for players
module Game.Unit.Player
  ( LevellingStats,
    Player,
    makePlayer,
  )
where

import Control.Lens
import Control.Monad.Free
import Game.Modifiers.EffectAtom
import Game.Modifiers.Modifier
import Game.Unit.Inventory (getAllWearableModifiers)
import Game.Unit.Stats (health)
import Game.Unit.TimedModifiers
import Game.Unit.Unit

-- | Describes everything regarding level-up system of a 'Player'
data LevellingStats
  = LevellingStats {_experienceLens :: Int, _skillPointsLens :: Int}

makeLenses ''LevellingStats

experience = _experienceLens

skillPoints = _skillPointsLens

-- | A unit that can get experience points and level-ups. Controlled from the outside world.
data Player = Player {_playerUnitLens :: Unit.UnitData, _levellingLens :: LevellingStats}

makeLenses ''Player

playerUnit = _playerUnitLens

levelling = _levellingLens

makePlayer :: Unit.UnitData -> Player
makePlayer unitData = Player unitData (LevellingStats 0 0)

instance Unit Player where
  asUnitData = _playerUnit

  applyModifier (Pure _) m = m
  applyModifier (Free (GetStats nextF)) m =
    applyModifier (nextF (Just $ m ^. playerUnit . stats)) m
  applyModifier (Free (SetStats newStats next)) u =
    applyModifier next (u & playerUnit . stats .~ newStats)
  applyModifier (Free (ModifyStats f next)) u =
    applyModifier next (u & playerUnit . stats %~ f)
  applyModifier (Free (SetTimedModifier time modifier next)) u =
    applyModifier next $
      over (playerUnit . timedModifiers) (addModifier time modifier) u
  applyModifier (Free (MoveTo coordTo next)) u =
    applyModifier next $ playerUnit . position .~ coordTo $ u
  applyModifier (Free (ApplyEffect effect next)) u =
    applyModifier next $ applyEffect effect u
    where
      applyEffect (Damage dmg) = playerUnit . stats . health %~ subtract dmg
      applyEffect (Heal h) = playerUnit . stats . health %~ (+) h
      applyEffect (GiveExp _) = id