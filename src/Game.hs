{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Game where

import           UI
import           Control.Lens

newtype Game = GameData { __currentUI :: UI Game }

makeLenses ''Game

currentUI :: Game -> UI Game
currentUI = __currentUI

updateUI :: UI Game -> Game -> Game
updateUI = set _currentUI