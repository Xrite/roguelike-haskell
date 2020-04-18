module Game.Controller.Scenario where

import Game.Scenario
import Game.Environment
import Control.Monad.Free


performScenario :: Scenario a -> Environment -> Environment
performScenario (Pure _) env = env 

performScenario (Free (Attack attacker attacked next)) env =  undefined 

performScenario (Free (MoveUnitTo uid coord next)) env =  undefined 

performScenario (Free (AOEUnitOp radius f next)) env =  undefined 