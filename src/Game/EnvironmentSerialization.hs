module Game.EnvironmentSerialization () where

import Game.Environment (EnvMemento, Position)
import Data.Binary (Binary (..))
import System.Random (StdGen)
import Game.Unit (Player, Mob, UnitData)
import Game.GameLevels.GameLevel (GameLevel)

instance Binary StdGen where
  put = put . show
  get = (read :: String -> StdGen) <$> get

instance Binary EnvMemento 
