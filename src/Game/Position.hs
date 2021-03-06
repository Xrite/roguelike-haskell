{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Game.Position where

import Control.Lens
import GHC.Generics (Generic)
import Data.Binary (Binary)

-- | Position in the environment
data Position
  = Position
      { _posLevel :: Int,
        _posX :: Int,
        _posY :: Int
      }
      deriving (Eq, Ord, Generic, Show)

makeLenses ''Position

instance Binary Position

-- | (x, y) coordinates of the position
positionXY :: Position -> (Int, Int)
positionXY pos = (pos ^. posX, pos ^. posY) 

uncheckedPosition :: Int -> (Int, Int) -> Position
uncheckedPosition l xy =
  Position
    { _posLevel = l,
      _posX = fst xy,
      _posY = snd xy
    }
