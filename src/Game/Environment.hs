{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Game.Environment
  ( Environment
  , UnitId(..)
  , makeEnvironment
  , makeUnitId
  , getCurrentLevel
  , unitById
  , unitLensById
  , unitByCoord
  , unitIdByCoord
  , affectUnitById
  , envAttack
  , playerId
  )
where

import qualified Game.Unit.Player as Player
import qualified Game.Unit.Mob as Mob
import qualified Game.Unit.Unit as Unit
import qualified Game.GameLevels.GameLevel as GameLevel
import           Control.Monad.State
import           Control.Lens
import           PreludeUtil (listLens)
import           Data.Foldable (find)
import           Data.List (findIndex)
import qualified Game.Effect as Effect
import           Game.Unit.DamageCalculation (attack)
import           Game.Unit.Stats
import qualified Game.Scenario as Scenario
import           Control.Monad.Free


-- | All manipulations with units in environment should use this type
newtype UnitId = UnitId Int

-- | Creates 'UnitId' from unit number in the list of units.
-- Created for testing purposes, avoid using at all costs.
makeUnitId :: Int -> UnitId
makeUnitId = UnitId

-- TODO maybe extract units to a different module?
-- TODO comment
data Environment =
   Environment { _player :: Player.Player, _units :: [Unit.AnyUnit], _levels :: [GameLevel.GameLevel], _currentLevel :: Int, _currentUnitTurn :: Int }
makeLenses ''Environment

instance Show Environment where
  show _ = "Environment"

-- | Constructs a new 'Environment'.
makeEnvironment :: Player.Player -> [Unit.AnyUnit] -> [GameLevel.GameLevel] -> Environment
makeEnvironment player units levels = Environment player units levels 0 0

-- | This function should remove dead units from environment.
-- It is called after each function that can modify units in the environment. With current implementation of units storage it invalidates 'UnitId'.
-- Item drop (units death effects in general) is not yet implemented, so TODO implement death effects in filterDead
filterDead :: Environment -> Environment
filterDead env = cycleCurrentUnit . (currentUnitTurn %~ flip (-) startUnitsDied) . (units .~ newUnits) $ env
  where
    startUnits = take (_currentUnitTurn env) $ _units env
    endUnits = drop (_currentUnitTurn env) $ _units env
    filterAlive = filter ((> 0) . _health . _stats . Unit.asUnitData)
    newStartUnits = filterAlive startUnits
    newEndUnits = filterAlive endUnits
    startUnitsDied = length startUnits - length newStartUnits
    newUnits = newStartUnits ++ newEndUnits

cycleCurrentUnit :: Environment -> Environment
cycleCurrentUnit env = 
  if _currentUnitTurn env == (length . _units) env
    then env { _currentUnitTurn = 0 }
    else env


unitLensById :: UnitId -> Lens' Environment Unit.AnyUnit
unitLensById (UnitId idxInt) = units . listLens idxInt

unitById :: UnitId -> Environment -> Unit.AnyUnit
unitById idx env = env ^. unitLensById idx

setUnitById :: UnitId -> Unit.AnyUnit -> Environment -> Environment
setUnitById idx unit = filterDead . set (unitLensById idx) unit

affectUnitById :: UnitId -> Effect.Effect () -> Environment -> Environment
affectUnitById idx effect = filterDead . (unitLensById idx %~ Unit.applyEffect effect)

unitIdByCoord :: (Int, Int) -> Environment -> Maybe UnitId
unitIdByCoord coord env = UnitId <$> findIndex ((== coord) . _position . Unit.asUnitData) (_units env)

unitByCoord :: (Int, Int) -> Environment -> Maybe Unit.AnyUnit
unitByCoord coord env = find ((== coord) . _position . Unit.asUnitData) $ _units env

envAttack :: UnitId -> UnitId -> Environment -> Environment
envAttack attackerId attackedId env = filterDead $ applyAttack env
  where
    attacker = unitById attackerId env
    attacked = unitById attackedId env
    (attackerNew, attackedNew) = attack attacker attacked
    applyAttack = (unitLensById attackerId .~ attackerNew) . (unitLensById attackedId .~ attackedNew)

newtype GameEnv a = GameEnv (State Environment a) deriving (Functor, Applicative, Monad, MonadState Environment)

class (Monad m) => GameEnvironmentReader m where
    getCurrentGameLevel :: m GameLevel.GameLevel

instance GameEnvironmentReader GameEnv where
  getCurrentGameLevel = do
    env <- get
    let cur = _currentLevel env
    return $ _levels env !! cur

getCurrentLevel :: Environment -> GameLevel.GameLevel
getCurrentLevel env = _levels env !! _currentLevel env

playerId :: Environment -> UnitId
playerId _ = UnitId 0

performScenario :: Scenario.Scenario UnitId a -> Environment -> (Environment, a)
performScenario (Pure value) env = (env, value)

performScenario (Free (Scenario.ApplyEffect effect unit next)) env = performScenario next $ over unitLens (Unit.applyEffect effect) env
  where
    unitLens = unitLensById

performScenario (Free (Scenario.MoveUnitTo uid coord next)) env =  performScenario next $ over unitLens (Unit.applyEffect effect) env
  where
    unitLens = unitLensById unit
    effect = Effect.setPosition coord

performScenario (Free (Scenario.AOEEffect radius effectByDistance next)) env =  undefined 

performScenario (Free (Scenario.GetUnitByPosition position nextF)) env =  performScenario (nextF unit) env
  where
    unit = unitIdByCoord position