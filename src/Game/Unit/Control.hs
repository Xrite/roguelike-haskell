module Game.Unit.Control where

import {-# SOURCE #-} Game.Environment
import Game.Unit.Action
import {-# SOURCE #-} Game.Unit.Unit

data TaggedControl = Aggressive | Passive | Avoiding

getControl :: TaggedControl -> Mob -> GameEnv Action
getControl Aggressive = aggressiveControl
getControl Passive = passiveControl
getControl Avoiding = avoidingControl

aggressiveControl :: Mob -> GameEnv Action
aggressiveControl mob = return $ moveDown

passiveControl :: Mob -> GameEnv Action
passiveControl mob = return $ moveDown

avoidingControl :: Mob -> GameEnv Action
avoidingControl mob = return $ moveDown
