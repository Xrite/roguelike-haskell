{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Game.Environment
  ( Environment
  )
where

import           Game.Unit.Player               ( Player )
import           Game.Unit.Mob                  ( Mob )
import           Game.Unit.Unit                 ( AnyUnit )
import           Game.GameLevels.GameLevel
import           Control.Monad.State
import           Control.Lens                   (makeLenses)

import Game.Unit.Unit
import Control.Lens ((^.), (%~))
import Game.GameLevels.GameLevel
import Data.Foldable (find)
import Game.GameLevels.MapCell
import Game.GameLevels.MapCellType
import Data.Maybe (isNothing)
import Game.Effect
import PreludeUtil


data Environment =
   Environment { _player :: Player, _units :: [AnyUnit], _levels :: [GameLevel], _currentLevel :: Int }
makeLenses ''Environment


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

-- | Moves a unit to a place if they can go there. Returns 'Nothing' if it is occupied by someone else or if it is a wall.
moveUnit :: Int -> (Int, Int) -> Environment -> Maybe Environment
moveUnit unitNumber coord env =
  if cantMoveThere
    then Nothing
    else Just $ units %~ setAt unitNumber (applyEffect (setCoord coord) movingUnit) $ env
  where
    movingUnit = _units env !! unitNumber
    cantMoveThere = canGo movingUnit (_position $ asUnitData movingUnit) coord env

findByCoord :: (Int, Int) -> Environment -> Maybe AnyUnit
findByCoord coord env = find ((== coord) . _position . asUnitData) $ _units env

canGo :: AnyUnit -> (Int, Int) -> (Int, Int) -> Environment -> Bool
canGo unit oldCoord newCoord env = oldCoord == newCoord || placeFree
  where
      occupyingUnitMaybe = findByCoord newCoord env
      placeOccupied = isNothing occupyingUnitMaybe
      cell = getCell newCoord $ getCurrentLevel env ^. lvlMap
      placeFree = not placeOccupied && (cell ^. cellType . passable $ asUnitData unit ^. stats)
