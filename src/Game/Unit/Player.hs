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
import Game.Unit.Stats (health)
import Game.Unit.TimedModifiers
import Game.Unit.Unit

-- | Describes everything regarding level-up system of a 'Player'
data LevellingStats
  = LevellingStats {_experience :: Int, _skillPoints :: Int}

makeLenses ''LevellingStats

-- | A unit that can get experience points and level-ups. Controlled from the outside world.
data Player = Player {_playerUnit :: UnitData, _levelling :: LevellingStats}

makeLenses ''Player

makePlayer :: UnitData -> Player
makePlayer unitData = Player unitData (LevellingStats 0 0)

instance Unit Player where
  asUnitData = _playerUnit

  applyModifier (Pure res) m = (m, res)
  applyModifier (Free (GetStats nextF)) m =
    applyModifier (nextF (Just $ m ^. playerUnit . stats)) m
  applyModifier (Free (ModifyStats f next)) u =
    applyModifier next (u & playerUnit . stats %~ f)
  applyModifier (Free (GetPosition nextF)) m =
    applyModifier (nextF (m ^. playerUnit . position)) m
  applyModifier (Free (ModifyPosition f next)) u =
    applyModifier next (u & playerUnit . position %~ f)
  applyModifier (Free (SetTimedModifier time modifier next)) u =
    applyModifier next $
      over (playerUnit . timedModifiers) (addModifier time modifier) u
  applyModifier (Free (MoveTo coordTo next)) u =
    applyModifier next $ playerUnit . position .~ coordTo $ u
  applyModifier (Free (GetPortrait nextF)) u =
    applyModifier (nextF (u ^. playerUnit . portrait)) u
  applyModifier (Free (ApplyEffect effect next)) u =
    applyModifier next $ applyEffect effect u
    where
      applyEffect (Damage dmg) = playerUnit . stats . health %~ subtract dmg
      applyEffect (Heal h) = playerUnit . stats . health %~ (+) h
      applyEffect (GiveExp _) = id
