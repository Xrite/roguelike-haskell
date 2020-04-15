{-# LANGUAGE TemplateHaskell #-}

-- | Describes a 'Unit' type for mobs
module Game.Unit.Mob where

import           Control.Lens
import           Control.Monad.Free
import           Game.Effects.Manipulatsi
import           Game.Unit.TimedEffects
import           Game.Unit.Inventory            ( getAllWearableEffects )
import           Game.Unit.Unit                 ( UnitData
                                                , Unit(..)
                                                , timedEffects
                                                , stats
                                                , inventory
                                                , position
                                                )

-- | A mob is a simple computer-controlled 'Unit'.
data Mob = Mob {_unit :: UnitData}

makeLenses ''Mob

instance Unit Mob where
  asUnitData = _unit

  applyEffect (Pure _) m = m
  applyEffect (Free (GetStats nextF)) m =
    applyEffect (nextF (Just $ m ^. unit . stats)) m
  applyEffect (Free (SetStats newStats next)) u =
    applyEffect next (u & unit . stats .~ newStats)
  applyEffect (Free (ModifyStats f next)) u =
    applyEffect next (u & unit . stats %~ f)
  applyEffect (Free (SetTimedEffect time effect next)) u =
    applyEffect next $ over (unit . timedEffects) (addEffect time effect) u
  applyEffect (Free (MoveTo coordTo next)) u =
    applyEffect next $ unit . position .~ coordTo $ u
