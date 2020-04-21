module Game.Unit.Control where

import Control.Lens
import Data.Graph.Inductive
import Data.List
import {-# SOURCE #-} Game.Environment
import Game.GameLevels.GameLevel
import Game.Unit.Action
import {-# SOURCE #-} Game.Unit.Unit

data TaggedControl = Aggressive | Passive | Avoiding | DoNothing

getControl :: TaggedControl -> Mob -> GameEnv Action
getControl Aggressive = aggressiveControl
getControl Passive = passiveControl
getControl Avoiding = avoidingControl
getControl DoNothing = const $ return stayAtPosition

aggressiveControl :: Mob -> GameEnv Action
aggressiveControl mob = return $ moveDown

passiveControl :: Mob -> GameEnv Action
passiveControl mob = return $ moveDown

avoidingControl :: Mob -> GameEnv Action
avoidingControl mob = return $ moveDown
