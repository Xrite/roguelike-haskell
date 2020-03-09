{-# LANGUAGE TemplateHaskell #-}

module Game.Player where

import           Game.Effect
import           Control.Lens
import           Game.Unit

data Player = 
  Player { 
  -- |Corresponding Unit
  _playerUnit :: Unit
  -- |Something that's different about player 
  , _knowledge :: () 
  }
makeLenses ''Player

applyEffect :: Effect () -> Player -> Player
applyEffect effect = playerUnit %~ Game.Unit.applyEffect effect