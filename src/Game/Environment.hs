{-# LANGUAGE TemplateHaskell #-}
module Game.Environment where

import Game.GameLevels.GameLevel
import Game.Unit.Unit
import Game.Unit.Player
import Control.Lens (makeLenses)

-- | A whole game
data Environment =
  Environment { _player :: Player, _units :: [AnyUnit], _levels :: [GameLevel], _currentLevel :: Int }
makeLenses ''Environment

getCurrentLevel :: Environment -> GameLevel
getCurrentLevel env = _levels env !! _currentLevel env