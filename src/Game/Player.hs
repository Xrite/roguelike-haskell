{-# LANGUAGE TemplateHaskell #-}
module Game.Player where

import           Game.Effect
import           Control.Lens
<<<<<<< HEAD
import           Game.TimedEffects
import           Control.Monad.Free
import           Game.Inventory
import           Game.Unit

data Player = Player { _experience :: Int, _unit :: UnitData }
=======
import           Game.Unit

>>>>>>> 72234b413bf879a225933bfe8951771a3580c591


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

<<<<<<< HEAD
instance Unit Player where
  asUnit p = _unit p

  applyEffect (Pure _) p = p
  applyEffect (Free (GetStats nextF)) p = applyEffect (nextF (p ^. unit . stats)) p
  applyEffect (Free (SetStats newStats next)) p =
    applyEffect next (p & unit . stats .~ newStats)
  applyEffect (Free (ModifyStats f next)) p =
    applyEffect next (p & unit . stats %~ f )
  applyEffect (Free (SetTimedEffect time effect next)) p = applyEffect next
    $ over (unit .timedEffects) (addEffect time effect) p
=======
applyEffect :: Effect () -> Player -> Player
applyEffect effect = playerUnit %~ Game.Unit.applyEffect effect
>>>>>>> 72234b413bf879a225933bfe8951771a3580c591
