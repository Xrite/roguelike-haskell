module Game.EnvironmentSerialization () where

import Game.Environment (EnvMemento)
import Data.Binary (Binary (..))
import Game.Unit.UnitSerialization
import Game.GameLevels.GameLevelSerialization
import Game.ItemSerialization
import Game.Modifiers.EffectSerialization
import System.Random (StdGen)

instance Binary StdGen where
  put = put . show
  get = (read :: String -> StdGen) <$> get

instance Binary EnvMemento