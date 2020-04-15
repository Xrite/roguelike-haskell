{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}

module Game.GameLevels.Generation.GenerationUtil where

import Control.Lens (makeLenses, (^.))
import Control.Monad.State
import Game.GameLevels.Generation.RandomMonad
import System.Random

-- |A data type for coordinates in a level.
-- A type no one asked for and no one except this module uses.
data Coord =
  Coord
    { _xCoord :: Int      -- ^ x coordinate
    , _yCoord :: Int      -- ^ y coordinate
    }
  deriving (Show, Eq)

makeLenses ''Coord

-- |A data type representing a 2D half-closed interval on a grid.
-- '_toCoord' is not included in a space.
-- If any coordinate of '_fromCoord' is not lower lower than that of '_toCoord' then a space is empty.
data Space =
  Space
    { _fromCoord :: Coord -- ^ starting corner of a space
    , _toCoord :: Coord   -- ^ finishing corner of a space
    }
  deriving (Show, Eq)

makeLenses ''Space

-- |A data type representing a room in a level.
-- Both corners are included (so a room is a 2D closed interval).
-- If any coordinate of '_fromCorner' is not lower lower than that of '_toCorner' then a room is empty.
data Room =
  Room
    { _fromCorner :: Coord
    , _toCorner :: Coord
    }
  deriving (Show, Eq)

makeLenses ''Room

-- |A data type representing a hall on the level.
-- Both corners are included.
--
-- If a coordinate of '_finishCorner' is lower that that of '_startCorner' then IS IS STILL A VALID NON-EMPTY HALL.
-- Given this two rules, any hall is not empty.
data Hall =
  Hall
    { _startCorner :: Coord
    , _finishCorner :: Coord
    }
  deriving (Show, Eq)

makeLenses ''Hall

toPair :: Coord -> (Int, Int)
toPair (Coord x y) = (x, y)

instance Random Coord where
  randomR (Coord x1 y1, Coord x2 y2) = runState genCoord
    where
      genCoord = do
        x <- mRandomR (x1, x2)
        y <- mRandomR (y1, y2)
        return $ Coord x y
  
  random = runState rndCoord
    where
      rndCoord = do
        x <- generate random
        y <- generate random
        return $ Coord x y

spaceSize :: Space -> Coord
spaceSize (Space (Coord x1 y1) (Coord x2 y2)) = Coord (x2 - x1) (y2 - y1)

spaceSizeX :: Space -> Int
spaceSizeX s = spaceSize s ^. xCoord

spaceSizeY :: Space -> Int
spaceSizeY s = spaceSize s ^. yCoord

-- |Generates a subinterval of provided size.
genSubInterval ::
     (RandomGen g, Random a, Num a) => a -> (a, a) -> g -> ((a, a), g)
genSubInterval minSize (from, to) = runState generateInterval
  where
    generateInterval = do
      size <- mRandomR (minSize, to - from)
      start <- mRandomR (0, to - size - from)
      return (from + start, from + start + size)

-- |Generates random halls between tho rooms.
-- 
-- This implementation simply chooses a point in each room and connects them with two straight lines
-- (direction is random)
makeHalls :: (RandomMonad g m) => Room -> Room -> m [Hall]
makeHalls (Room from1 to1) (Room from2 to2) = do
  c1@(Coord x1 y1) <- mRandomR (from1, to1)
  c2@(Coord x2 y2) <- mRandomR (from2, to2)
  coinToss <- mRandomR (False, True)
  let middlePoint = if coinToss then Coord x1 y2 else Coord x2 y1
  return [Hall c1 middlePoint, Hall middlePoint c2]
