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
import Game.Modifiers.UnitOp
import Game.Unit.Stats (health)
import Game.Unit.TimedUnitOps
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

  applyUnitOp (Pure res) m = (m, res)
  applyUnitOp (Free (GetStats nextF)) m =
    applyUnitOp (nextF (Just $ m ^. playerUnit . stats)) m
  applyUnitOp (Free (ModifyStats f next)) u =
    applyUnitOp next (u & playerUnit . stats %~ f)
  applyUnitOp (Free (GetPosition nextF)) m =
    applyUnitOp (nextF (m ^. playerUnit . position)) m
  applyUnitOp (Free (ModifyPosition f next)) u =
    applyUnitOp next (u & playerUnit . position %~ f)
  applyUnitOp (Free (SetTimedUnitOp time modifier next)) u =
    applyUnitOp next $
      over (playerUnit . timedUnitOps) (addUnitOp time modifier) u
  applyUnitOp (Free (MoveTo coordTo next)) u =
    applyUnitOp next $ playerUnit . position .~ coordTo $ u
  applyUnitOp (Free (GetPortrait nextF)) u =
    applyUnitOp (nextF (u ^. playerUnit . portrait)) u
  applyUnitOp (Free (ApplyEffect effect next)) u =
    applyUnitOp next $ applyEffect effect u
    where
      applyEffect (Damage dmg) = playerUnit . stats . health %~ subtract (fromNonNegative dmg)
      applyEffect (Heal h) = playerUnit . stats . health %~ (+) (fromNonNegative h)
      applyEffect (GiveExp _) = id
