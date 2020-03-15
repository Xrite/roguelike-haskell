module Game.Environment where

import Game.GameLevels.GameLevel
import Game.Unit.Unit
import Game.Unit.Player

-- | A whole game
data Environment =
  Environment { _player :: Player, _units :: [AnyUnit], _levels :: [GameLevel], _currentLevel :: Int }
