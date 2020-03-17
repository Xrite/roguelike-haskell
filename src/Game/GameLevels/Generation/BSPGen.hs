{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable #-}
{-# LANGUAGE MonoLocalBinds #-}

module Game.GameLevels.Generation.BSPGen
  ( xCoord
  , yCoord
  , toCoord
  , fromCoord
  , fromCorner
  , toCorner
  , startCorner
  , finishCorner
  , generateLevel
  , Space (..)
  , Coord (..)
  , toPair
  , GeneratorParameters (..)
  ) where

import Control.Lens (Lens', (.~), (^.), makeLenses)
import Control.Monad.State
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Game.GameLevels.Generation.RandomMonad
import System.Random

-- |A data type for coordinates in a level.
-- A type no one asked for and no one except this module uses.
data Coord =
  Coord
    { _xCoord :: Int      -- ^ x coordinate
    , _yCoord :: Int      -- ^ y coordinate
    }
  deriving (Show)

makeLenses ''Coord

-- |A data type representing a 2D half-closed interval on a grid.
-- '_toCoord' is not included in a space.
-- If any coordinate of '_fromCoord' is not lower lower than that of '_toCoord' then a space is empty.
data Space =
  Space
    { _fromCoord :: Coord -- ^ starting corner of a space
    , _toCoord :: Coord   -- ^ finishing corner of a space
    }
  deriving (Show)

makeLenses ''Space

-- |A data type representing a room in a level.
-- Both corners are included (so a room is a 2D closed interval).
-- If any coordinate of '_fromCorner' is not lower lower than that of '_toCorner' then a room is empty.
data Room =
  Room
    { _fromCorner :: Coord
    , _toCorner :: Coord
    }
  deriving (Show)

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
  deriving (Show)

makeLenses ''Hall

-- |Internal type for a binary tree, used in a binary tree level generation algorithm.
data BTree a
  = Branch
      { _leftSon :: BTree a
      , _rightSon :: BTree a
      }
  | Leaf
      { _leafValue :: a
      }
  deriving (Eq, Show, Functor, Foldable)

makeLenses ''BTree

-- |Describes Parameters of level generation.
data GeneratorParameters =
  GeneratorParameters
    { -- | Minimal allowed size of a space in which a room is created to be generated.
      -- | It affects how many rooms will be generated. See link to the generation algorithm description in
      -- 'generateLevel' documentation
      minSpaceSize :: Int
      -- | Maximal allowed ratio of rooms sides
    , sizesRatioFix :: Float
      -- | Minimal size of a room.
      -- | Note that it must be strictly lower then 'minSpaceSize' to make room for walls.
    , minRoomSize :: Int
    }
  deriving (Show)

toPair :: Coord -> (Int, Int)
toPair (Coord x y) = (x, y)

-- |Generates a level as lists of rooms and halls using binary space tree algorithm.
-- (See <https://gamedevelopment.tutsplus.com/tutorials/how-to-use-bsp-trees-to-generate-game-maps--gamedev-12268 this article>)
--
-- Generated level is guaranteed to have following properties:
-- * All rooms are at least as big and have a sides ratio not greater than as provided 'GeneratorParameters' specifies.
-- * All halls have one dimension of size 1 (but multiple halls may spawn in parallel)
--   If initial space was big enough to fit one room. It is unspecified what will happen otherwise.
--
-- If parameters are inconsistent in any way, behavior of this function is unspecified.
generateLevel :: (RandomMonad g m)
              => GeneratorParameters   -- ^ Parameters of a level
              -> Space                 -- ^ Space in which a level should be contained
              -> m ([Room], [Hall])    -- ^ All rooms and halls of a generated level in a random monad of choice
generateLevel param s = do
  spaceTree <- generate $ generateSpaceTree param s
  (_, halls) <- generateHalls param spaceTree
  return (foldMap return spaceTree, halls)

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

-- |Splits provided space randomly in two such that both halves are bigger than '_minSpaceSize' of provided parameters.
-- Returns 'Nothing' if it is impossible.
splitSpace ::
     RandomGen g
  => GeneratorParameters
  -> Space
  -> g
  -> Maybe ((Space, Space), g)
splitSpace param s gen =
  if splitFrom > splitTo
    then Nothing
    else Just $ flip runState gen' $ do
           splitValueRelative <- mRandomR (splitFrom, splitTo)
           let splitValue = s ^. fromCoord . splitLens + splitValueRelative
           return ((toCoord . splitLens) .~ splitValue $ s, (fromCoord . splitLens) .~ splitValue $ s)
  where
    ratio :: Float
    ratio = fromIntegral (spaceSizeX s) / fromIntegral (spaceSizeY s)
    (splitX, gen')
      | spaceSizeX s < 2 * minSpaceSize param = (False, gen)
      | spaceSizeY s < 2 * minSpaceSize param = (True, gen)
      | ratio > sizesRatioFix param = (True, gen)
      | 1 > ratio * sizesRatioFix param = (False, gen)
      | otherwise = randomR (False, True) gen
    splitLens :: Lens' Coord Int
    splitLens =
      if splitX
        then xCoord
        else yCoord
    splitFrom = minSpaceSize param
    splitTo = (spaceSize s ^. splitLens) - minSpaceSize param

-- |Generates a subinterval of provided size.
genSubInterval ::
     (RandomGen g, Random a, Num a) => a -> (a, a) -> g -> ((a, a), g)
genSubInterval minSize (from, to) = runState generateInterval
  where
    generateInterval = do
      size <- mRandomR (minSize, to - from)
      start <- mRandomR (0, to - size - from)
      return (from + start, from + start + size)

-- |Generates a random big enough room in provided space. If it is impossible, behavior is unspecified.
roomIn :: RandomGen g => GeneratorParameters -> g -> Space -> (Room, g)
roomIn param g (Space (Coord x1 y1) (Coord x2 y2)) = runState genRoom g
  where
    genRoom = do
      (resX1, resX2) <- generate $ genSubInterval minSize (x1, x2)
      (resY1, resY2) <- generate $ genSubInterval minSize (y1, y2)
      return $ Room (Coord resX1 resY1) (Coord (resX2 - 2) (resY2 - 2))
    minSize = minRoomSize param

-- |Generates a binary space tree of big enough spaces.
generateSpaceTree ::
     RandomGen g => GeneratorParameters -> Space -> g -> (BTree Room, g)
generateSpaceTree param s gen =
  fromMaybe (first Leaf $ roomIn param gen s) $ runStateT maybeAns gen
  where
    maybeAns = do
      ((leftSpace, rightSpace), g') <- lift $ splitSpace param s gen
      put g'
      leftTree <- generate $ generateSpaceTree param leftSpace
      rightTree <- generate $ generateSpaceTree param rightSpace
      return $ Branch leftTree rightTree

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

-- |Generates halls based on a binary tree of rooms
generateHalls
  :: RandomMonad g m
  => GeneratorParameters
  -> BTree Room -> m (Room, [Hall])
generateHalls _ tree = generateHallsHelper tree []
  where
    generateHallsHelper :: (RandomMonad g m) => BTree Room -> [Hall] -> m (Room, [Hall])
    generateHallsHelper (Leaf s) halls = return (s, halls)
    generateHallsHelper (Branch leftT rightT) halls = do
      (leftRoom, halls') <- generateHallsHelper leftT halls
      (rightRoom, halls'') <- generateHallsHelper rightT halls'
      newHalls <- makeHalls leftRoom rightRoom
      coinToss <- mRandomR (False, True)
      let returnRoom =
            if coinToss
              then leftRoom
              else rightRoom
      return (returnRoom, newHalls ++ halls'')