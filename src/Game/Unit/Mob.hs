{-# LANGUAGE TemplateHaskell #-}

-- | Describes a 'Unit' type for mobs
module Game.Unit.Mob where

import           Control.Lens
import           Control.Monad.Free
import qualified Game.Effect as Effect
import qualified Game.Unit.TimedEffects as TimedEffects
import qualified Game.Unit.Inventory as Inventory
import qualified Game.Unit.Unit as Unit

-- | A mob is a simple computer-controlled 'Unit'.
data Mob = Mob { _unitLens :: Unit.UnitData }

makeLenses ''Mob

unit = _unitLens

instance Unit.Unit Mob where
  asUnitData = _unit

  applyEffect (Pure value) m = (m, value)
  applyEffect (Free (Effect.GetStats nextF)) m =
    Unit.applyEffect (nextF (Just $ m ^. unitLens . Unit.statsLens)) m
  applyEffect (Free (Effect.ModifyStats f next)) u =
    Unit.applyEffect next (u & unitLens . Unit.statsLens %~ f)
  applyEffect (Free (Effect.SetTimedEffect time effect next)) u =
    Unit.applyEffect next
    $ over
      (unitLens . Unit.timedEffectsLens)
      (TimedEffects.addEffect time effect)
      u
  applyEffect (Free (Effect.GetPosition nextF)) m =
    Unit.applyEffect (nextF $ m ^. unitLens . Unit.positionLens) m
  applyEffect (Free (Effect.ModifyPosition f next)) u = Unit.applyEffect next
    $ over (unitLens . Unit.positionLens) f u
  applyEffect (Free (Effect.GetLevelDepth nextF)) m =
    Unit.applyEffect (nextF $ m ^. unitLens . Unit.depthLens) m
  applyEffect (Free (Effect.ModifyLevelDepth f next)) m = Unit.applyEffect next
    $ over (unitLens . Unit.depthLens) f m