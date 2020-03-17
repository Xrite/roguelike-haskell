{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Game.Environment
  ( Environment(..)
  , getCurrentLevel
  , units
  )
where

import           Game.Unit.Player               ( Player )
import           Game.Unit.Mob                  ( Mob )
import           Game.Unit.Unit                 ( AnyUnit )
import           Game.GameLevels.GameLevel
import           Control.Monad.State
import           Control.Lens                   (makeLenses)


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
