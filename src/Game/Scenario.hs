{-# LANGUAGE DeriveFunctor #-}

module Game.Scenario where

import           Control.Monad.Free
import           Game.Effects.Manipulatsi (ProizvolnueManipulatsi)
import           Game.Environment


data ScenarioDSL a = Attack UnitId UnitId a
                   | MoveUnitTo UnitId (Int, Int) a
                   | AOEEffect Int (Int -> ProizvolnueManipulatsi ()) a
  deriving (Functor)

type Scenario a = Free ScenarioDSL a

attack :: UnitId -> UnitId -> Scenario ()
attack attacker attacked = liftF $ Attack attacker attacked ()

setAOEEffect :: Int -> (Int -> ProizvolnueManipulatsi ()) -> Scenario ()
setAOEEffect range effect = liftF $ AOEEffect range effect ()

setCoord :: UnitId -> (Int, Int) -> Scenario ()
setCoord uid coord = liftF $ MoveUnitTo uid coord ()