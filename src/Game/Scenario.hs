{-# LANGUAGE DeriveFunctor #-}

module Game.Scenario where

import           Control.Monad.Free
import           Game.Effect (Effect)

data ScenarioDSL unitAccessor a = ApplyEffect (Effect ()) unitAccessor a
                                | MoveUnitTo unitAccessor (Int, Int) a
                                | AOEEffect Int (Int -> Effect ()) a
                                | GetUnitByCoord (Int, Int) (Maybe unitAccessor -> a)
  deriving (Functor)

type Scenario unitAccessor a = Free (ScenarioDSL unitAccessor) a

applyEffect :: Effect () -> unitAccessor -> Scenario unitAccessor ()
applyEffect effect unit = liftF $ ApplyEffect effect unit ()

setAOEEffect :: Int -> (Int -> Effect ()) -> Scenario unitAccessor ()
setAOEEffect range effect = liftF $ AOEEffect range effect ()

setCoord :: unitAccessor -> (Int, Int) -> Scenario unitAccessor ()
setCoord unit coord = liftF $ MoveUnitTo unit coord ()

getUnitByCoord :: (Int, Int) -> Scenario unitAccessor (Maybe unitAccessor)
getUnitByCoord position = liftF $ GetUnitByCoord position id
