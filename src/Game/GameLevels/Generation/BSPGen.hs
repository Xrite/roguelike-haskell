{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable #-}
{-# LANGUAGE MonoLocalBinds #-}

-- | Describes level generation using binary space tree algorithm.
module Game.GameLevels.Generation.BSPGen
  ( generateLevel
  , GeneratorParameters (..)
  ) where

import Game.GameLevels.Generation.GenerationUtil
import Control.Lens (Lens', (.~), (^.), makeLenses)
import Control.Monad.State
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Game.GameLevels.Generation.RandomMonad
import System.Random

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
              -> Space                 -- ^ Space in which a level should be contained (should be STRICTLY bigger than minimal room size)
              -> m ([Room], [Hall])    -- ^ All rooms and halls of a generated level in a random monad of choice
generateLevel param s = do
  spaceTree <- generate $ generateSpaceTree param s
  (_, halls) <- generateHalls param spaceTree
  return (foldMap return spaceTree, halls)

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