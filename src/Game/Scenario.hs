{-# LANGUAGE DeriveFunctor #-}

module Game.Scenario where

import           Control.Monad.Free
import           Game.Modifiers.Modifier (Modifier)
import           Game.Environment


data ScenarioDSL a = Attack UnitId UnitId a
                   | MoveUnitTo UnitId (Int, Int) a
                   | AOEModifier Int (Int -> Modifier ()) a
  deriving (Functor)

type Scenario a = Free ScenarioDSL a

attack :: UnitId -> UnitId -> Scenario ()
attack attacker attacked = liftF $ Attack attacker attacked ()

setAOEModifier :: Int -> (Int -> Modifier ()) -> Scenario ()
setAOEModifier range modifier = liftF $ AOEModifier range modifier ()

setCoord :: UnitId -> (Int, Int) -> Scenario ()
setCoord uid coord = liftF $ MoveUnitTo uid coord ()