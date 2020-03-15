{-# LANGUAGE TemplateHaskell #-}

module Game.Unit.Mob where

import           Control.Lens
import           Control.Monad.Free
import           Game.Effect
import           Game.Unit.TimedEffects
import           Game.Unit.Inventory            ( getAllWearableEffects
                                                , getWeaponEffect
                                                )
import           Game.Unit.Unit                 ( UnitData , Unit(..), timedEffects, stats, inventory, position)

data Mob = Mob {_unit :: UnitData}

makeLenses ''Mob

instance Unit Mob where
  asUnitData = _unit

  applyEffect (Pure _) m = m
  applyEffect (Free (GetStats nextF)) m =
    applyEffect (nextF (m ^. unit . stats)) m
  applyEffect (Free (SetStats newStats next)) u =
    applyEffect next (u & unit . stats .~ newStats)
  applyEffect (Free (ModifyStats f next)) u =
    applyEffect next (u & unit . stats %~ f)
  applyEffect (Free (SetTimedEffect time effect next)) u =
    applyEffect next $ over (unit . timedEffects) (addEffect time effect) u
  applyEffect (Free (MoveTo coordTo next)) u =
    applyEffect next $ unit . position .~ coordTo $ u

  attackEffect p =
    getWeaponEffect $ applyEffect wearableEff p ^. unit . inventory
   where
    inv         = p ^. unit . inventory
    wearableEff = getAllWearableEffects inv

--makeMob :: Stats -> (GameLevel -> [AnyUnit] -> Action) -> AnyUnit
--makeMob stat ai =
--  Mob $ createUnitData stat (TimedEffects []) $ \lvl units -> pureGameIO (ai lvl units)
