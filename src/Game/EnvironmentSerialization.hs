module Game.EnvironmentSerialization () where

import Game.Environment (EnvMemento, Position)
import Data.Binary (Binary (..))
import System.Random (StdGen)

{- instance Binary StdGen where
  put = put . show
  get = (read :: String -> StdGen) <$> get

instance Binary Position

instance Binary EnvMemento -}