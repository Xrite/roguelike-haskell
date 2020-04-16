{-# LANGUAGE DeriveFunctor #-}

module Game.Scenario where

import           Control.Monad.Free
import           Game.Modifiers.Modifier (Modifier)
import           Game.Environment


data ScenarioDSL a = Attack UnitId UnitId a
                   | MoveUnitTo UnitId (Int, Int) a
                   | AOEModifier Int (Int -> Modifier ()) a
  deriving (Functor)

type Scenario unitAccessor a = Free (ScenarioDSL unitAccessor) a

applyEffect :: Effect () -> unitAccessor -> Scenario unitAccessor ()
applyEffect effect unit = liftF $ ApplyEffect effect unit ()

setAOEModifier :: Int -> (Int -> Modifier ()) -> Scenario ()
setAOEModifier range modifier = liftF $ AOEModifier range modifier ()

setCoord :: unitAccessor -> (Int, Int) -> Scenario unitAccessor ()
setCoord unit coord = liftF $ MoveUnitTo unit coord ()

getUnitByCoord :: (Int, Int) -> Scenario unitAccessor (Maybe unitAccessor)
getUnitByCoord position = liftF $ GetUnitByCoord position id
