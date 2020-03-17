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
import Util


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
moveUnit :: Environment -> Int -> (Int, Int) -> Maybe Environment
moveUnit env unitNumber coord =
  if cantMoveThere
    then Nothing
    else Just $ units %~ setAt unitNumber (applyEffect (setCoord coord) movingUnit) $ env
  where
    movingUnit = _units env !! unitNumber
    cantMoveThere = canGo env movingUnit (_position $ asUnitData movingUnit) coord

findByCoord :: Environment -> (Int, Int) -> Maybe AnyUnit
findByCoord env coord = find ((== coord) . _position . asUnitData) $ _units env

canGo :: Environment -> AnyUnit -> (Int, Int) -> (Int, Int) -> Bool
canGo env unit oldCoord newCoord = oldCoord == newCoord || canMoveThere
  where
      occupyingUnitMaybe = findByCoord env newCoord
      placeOccupied = isNothing occupyingUnitMaybe
      cell = getCell newCoord $ getCurrentLevel env ^. lvlMap
      canMoveThere = not placeOccupied && (cell ^. cellType . passable $ asUnitData unit ^. stats)