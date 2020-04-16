{-# LANGUAGE TemplateHaskell #-}

-- | Describes a 'Unit' type for players 
module Game.Unit.Player (LevellingStats, Player, makePlayer) where

import qualified Game.Effect as Effect
import           Control.Lens 
import qualified Game.Unit.Unit as Unit
import           Control.Monad.Free
import qualified Game.Unit.TimedEffects as TimedEffects
import qualified Game.Unit.Inventory as Inventory
import qualified Game.Scenario as Scenario

-- | Describes everything regarding level-up system of a 'Player'
data LevellingStats =
  LevellingStats { _experienceLens :: Int, _skillPointsLens :: Int }

makeLenses ''LevellingStats

experience = _experienceLens

skillPoints = _skillPointsLens

-- | A unit that can get experience points and level-ups. Controlled from the outside world.
data Player = Player { _playerUnitLens :: Unit.UnitData, _levellingLens :: LevellingStats }

makeLenses ''Player

playerUnit = _playerUnitLens

levelling = _levellingLens

makePlayer :: Unit.UnitData -> Player
makePlayer unitData = Player unitData (LevellingStats 0 0)

instance Unit.Unit Player where
  asUnitData = playerUnit

  applyEffect (Pure value) p = (p, value)
  applyEffect (Free (Effect.GetStats nextF)) p =
    Unit.applyEffect (nextF (Just $ p ^. playerUnitLens . Unit.statsLens)) p
  applyEffect (Free (Effect.ModifyStats f next)) p =
    Unit.applyEffect next (p & playerUnitLens . Unit.statsLens %~ f)
  applyEffect (Free (Effect.SetTimedEffect time effect next)) p = 
    Unit.applyEffect next
    $ over (playerUnitLens . Unit.timedEffectsLens) (TimedEffects.addEffect time effect) p
  applyEffect (Free (Effect.GetPosition nextF)) p = Unit.applyEffect (nextF $ p ^. playerUnitLens . Unit.positionLens) p
  applyEffect (Free (Effect.ModifyPosition f next)) u =
    Unit.applyEffect next $ over (playerUnitLens . Unit.positionLens) f u
  applyEffect (Free (Effect.GetLevelDepth nextF)) p = Unit.applyEffect (nextF $ p ^. playerUnitLens . Unit.depthLens) p
  applyEffect (Free (Effect.ModifyLevelDepth f next)) p = Unit.applyEffect next $ (over (playerUnitLens . Unit.depthLens) f p)

{-     where
      (newPlayer, _) = Unit.applyEffect wearableEff p ^. playerUnitLens . inventoryLens
      inv = newPlayer ^. playerUnit . inventoryLens
      wearableEff = getAllWearableEffects inv -}
