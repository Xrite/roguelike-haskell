{-# LANGUAGE DeriveFunctor #-}

module Game.Scenario where

import           Control.Monad.Free
import           Game.Modifiers.UnitOp (UnitOp)
import           Game.Environment


data ScenarioDSL a = Attack UnitId UnitId a
                   | MoveUnitTo UnitId (Int, Int) a
                   | AOEUnitOp Int (Int -> UnitOp ()) a
  deriving (Functor)

type Scenario a = Free ScenarioDSL a

attack :: UnitId -> UnitId -> Scenario ()
attack attacker attacked = liftF $ Attack attacker attacked ()

setAOEUnitOp :: Int -> (Int -> UnitOp ()) -> Scenario ()
setAOEUnitOp range modifier = liftF $ AOEUnitOp range modifier ()

setCoord :: UnitId -> (Int, Int) -> Scenario ()
setCoord uid coord = liftF $ MoveUnitTo uid coord ()