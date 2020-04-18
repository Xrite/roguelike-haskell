{-# LANGUAGE TemplateHaskell #-}

-- | Describes a 'Unit' type for players 
module Game.Unit.Player (LevellingStats, Player, makePlayer) where

import           Game.Modifiers.UnitOp
import           Control.Lens
import           Game.Unit.Unit
import           Control.Monad.Free
import           Game.Unit.TimedUnitOps
import           Game.Unit.Inventory (getAllWearableUnitOps)
import Game.Modifiers.EffectAtom
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

  applyUnitOp (Pure _) m = m
  applyUnitOp (Free (GetStats nextF)) m =
    applyUnitOp (nextF (Just $ m ^. playerUnit . stats)) m
  applyUnitOp (Free (SetStats newStats next)) u =
    applyUnitOp next (u & playerUnit . stats .~ newStats)
  applyUnitOp (Free (ModifyStats f next)) u =
    applyUnitOp next (u & playerUnit . stats %~ f)
  applyUnitOp (Free (SetTimedUnitOp time modifier next)) u = applyUnitOp next
    $ over (playerUnit . timedUnitOps) (addUnitOp time modifier) u
  applyUnitOp (Free (MoveTo coordTo next)) u =
    applyUnitOp next $ playerUnit . position .~ coordTo $ u
  applyUnitOp (Free (ApplyEffect effect next)) u =
    applyUnitOp next $ applyEffect effect u
    where
      applyEffect (Damage dmg) = playerUnit . stats . health %~ subtract dmg
      applyEffect (Heal h) = playerUnit . stats . health %~ (+) h
      applyEffect (GiveExp _) = id
