{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Game.Environment
  ( Environment
  )
where

import           Game.Unit.Player               ( Player )
import           Game.Unit.Mob                  ( Mob )
import           Game.GameLevels.GameLevel
import           Control.Monad.State


data Environment =
  Environment { _player :: Player, _mobs :: [Mob], _levels :: [GameLevel], _currentLevel :: Int}

newtype GameEnv a = GameEnv (State Environment a) deriving (Functor, Applicative, Monad, MonadState Environment)

class (Monad m) => GameEnvironmentReader m where
    getCurentGameLevel :: m GameLevel

instance GameEnvironmentReader GameEnv where
  getCurentGameLevel = do
    env <- get
    let cur = _currentLevel env
    return $ _levels env !! cur