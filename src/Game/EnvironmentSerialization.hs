module Game.EnvironmentSerialization () where

import Data.Binary (Binary (..))
import Game.Environment (EnvMemento, MobId, PlayerId, Position, UnitId)
import Game.GameLevels.GameLevel (GameLevel)
import Game.GameLevels.GameLevelSerialization
import Game.Unit.UnitSerialization
import System.Random (StdGen)

instance Binary StdGen where
  put = put . show
  get = (read :: String -> StdGen) <$> get

instance Binary EnvMemento

instance Binary UnitId

instance Binary MobId

instance Binary PlayerId
