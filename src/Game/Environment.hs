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
  , renderEnvironment
  )
where

import           Game.Unit.Player               ( Player )
import           Game.Unit.Mob                  ( Mob )
import           Game.Unit.Unit                 ( AnyUnit, asUnitData, _position, applyModifier, stats, _stats, position, _portrait )
import           Game.GameLevels.GameLevel
import           Control.Monad.State
import           Control.Lens
import           PreludeUtil                    ( listLens )
import           Data.Foldable                  ( find )
import           Data.List                      ( findIndex )
import Game.Modifiers.Modifier (Modifier)
import Game.Unit.DamageCalculation (attack)
import Game.Unit.Stats
import Game.GameLevels.MapCell (renderCell)

-- | All manipulations with units in environment should use this type
newtype UnitId = UnitId Int

-- | Creates 'UnitId' from unit number in the list of units.
-- Created for testing purposes, avoid using at all costs.
makeUnitId :: Int -> UnitId
makeUnitId = UnitId

-- TODO maybe extract units to a different module?
-- TODO comment
data Environment =
   Environment { _player :: Player, _units :: [AnyUnit], _levels :: [GameLevel], _currentLevel :: Int, _currentUnitTurn :: Int }
makeLenses ''Environment

instance Show Environment where
  show _ = "Environment"

-- | Constructs a new 'Environment'.
makeEnvironment :: Player -> [AnyUnit] -> [GameLevel] -> Environment
makeEnvironment player units levels = Environment player units levels 0 0

-- | This function should remove dead units from environment.
-- It is called after each function that can modify units in the environment. With current implementation of units storage it invalidates 'UnitId'.
-- Item drop (units death modifiers in general) is not yet implemented, so TODO implement death modifiers in filterDead
filterDead :: Environment -> Environment
filterDead env = cycleCurrentUnit . (currentUnitTurn %~ subtract startUnitsDied) . (units .~ newUnits) $ env
  where
    startUnits = take (_currentUnitTurn env) $ _units env
    endUnits = drop (_currentUnitTurn env) $ _units env
    filterAlive = filter ((> 0) . _health . _stats . asUnitData)
    newStartUnits = filterAlive startUnits
    newEndUnits = filterAlive endUnits
    startUnitsDied = length startUnits - length newStartUnits
    newUnits = newStartUnits ++ newEndUnits

cycleCurrentUnit :: Environment -> Environment
cycleCurrentUnit env = 
  if _currentUnitTurn env == (length . _units) env
    then env { _currentUnitTurn = 0 }
    else env


unitLensById :: UnitId -> Lens' Environment AnyUnit
unitLensById (UnitId idxInt) = units . listLens idxInt

unitById :: UnitId -> Environment -> AnyUnit
unitById idx env = env ^. unitLensById idx

setUnitById :: UnitId -> AnyUnit -> Environment -> Environment
setUnitById idx unit = filterDead . set (unitLensById idx) unit

affectUnitById :: UnitId -> Modifier () -> Environment -> Environment
affectUnitById idx modifier = filterDead . (unitLensById idx %~ applyModifier modifier)

unitIdByCoord :: (Int, Int) -> Environment -> Maybe UnitId
unitIdByCoord coord env = UnitId <$> findIndex ((== coord) . _position . asUnitData) (_units env)

unitByCoord :: (Int, Int) -> Environment -> Maybe AnyUnit
unitByCoord coord env = find ((== coord) . _position . asUnitData) $ _units env

envAttack :: UnitId -> UnitId -> Environment -> Environment
envAttack attackerId attackedId env = filterDead $ applyAttack env
  where
    attacker = unitById attackerId env
    attacked = unitById attackedId env
    (attackerNew, attackedNew) = attack attacker attacked
    applyAttack = (unitLensById attackerId .~ attackerNew) . (unitLensById attackedId .~ attackedNew)

newtype GameEnv a = GameEnv (State Environment a) deriving (Functor, Applicative, Monad, MonadState Environment)

class (Monad m) => GameEnvironmentReader m where
    getCurrentGameLevel :: m GameLevel

instance GameEnvironmentReader GameEnv where
  getCurrentGameLevel = do
    env <- get
    let cur = _currentLevel env
    return $ _levels env !! cur

getCurrentLevel :: Environment -> GameLevel
getCurrentLevel env = _levels env !! _currentLevel env

playerId :: Environment -> UnitId
playerId _ = UnitId 0

renderEnvironment :: Environment -> [String]
renderEnvironment env = [[maybe (renderCell $ getCell (x, y) mp) (_portrait . asUnitData)  $ find (\u -> (asUnitData u ^. position) == (x, y)) $ _units env | x <- [xFrom .. xTo]]
                  | y <- [yFrom .. yTo]]
  where
    mp = getCurrentLevel env ^. lvlMap
    ((xFrom, yFrom), (xTo, yTo)) = getMapSize mp