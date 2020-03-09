{-# LANGUAGE TemplateHaskell #-}

module Game.Player where

import           Game.Effect
import           Control.Lens
import           Game.Unit



data LevellingStats = LevellingStats
  { _experience :: Int
  , _skillPoints :: Int
  }
makeLenses ''LevellingStats

data Player = 
  Player { 
  -- |Corresponding Unit
  _playerUnit :: Unit
  , _levelling :: LevellingStats
  }
makeLenses ''Player

applyEffect :: Effect () -> Player -> Player
applyEffect effect = playerUnit %~ Game.Unit.applyEffect effect