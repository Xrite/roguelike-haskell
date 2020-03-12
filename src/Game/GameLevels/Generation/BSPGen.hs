{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable #-}

module Game.GameLevels.Generation.BSPGen where

import Control.Lens (Lens', (.~), (^.), makeLenses)
import Control.Monad.State
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Game.GameLevels.Generation.RandomMonad
import System.Random

data Coord =
  Coord
    { _xCoord :: Int
    , _yCoord :: Int
    }
  deriving (Show)

makeLenses ''Coord

data Room =
  Room
    { _fromCorner :: Coord
    , _toCorner :: Coord
    }
  deriving (Show)

makeLenses ''Room

data Hall =
  Hall
    { _startCorner :: Coord
    , _finishCorner :: Coord
    }
  deriving (Show)

makeLenses ''Hall

data Space =
  Space
    { _fromCoord :: Coord
    , _toCoord :: Coord
    }
  deriving (Show)

makeLenses ''Space

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

data GeneratorParameters =
  GeneratorParameters
    { minSpaceSize :: Int
    , sizesRatioFix :: Float
    , minRoomSize :: Int
    }
  deriving (Show)

instance Random Coord where
  randomR ((Coord x1 y1), (Coord x2 y2)) g = runState genCoord g
    where
      genCoord = do
        x <- stRandomR (x1, x2)
        y <- stRandomR (y1, y2)
        return $ Coord x y
  
  random g = runState rndCoord g
    where
      rndCoord = do
        x <- generate $ random
        y <- generate $ random
        return $ Coord x y

spaceSize :: Space -> Coord
spaceSize (Space (Coord x1 y1) (Coord x2 y2)) = Coord (x2 - x1) (y2 - y1)

spaceSizeX :: Space -> Int
spaceSizeX s = spaceSize s ^. xCoord

spaceSizeY :: Space -> Int
spaceSizeY s = spaceSize s ^. yCoord

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
        splitValueRelative <- stRandomR (splitFrom, splitTo)
        let splitValue = s ^. fromCoord . splitLens + splitValueRelative
        return 
            ( (toCoord . splitLens) .~ splitValue $ s
            , (fromCoord . splitLens) .~ splitValue $ s)
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
    splitTo = ((spaceSize s) ^. splitLens) - minSpaceSize param

genSubInterval ::
     (RandomGen g, Random a, Num a) => a -> (a, a) -> g -> ((a, a), g)
genSubInterval minSize (from, to) = runState generateInterval
  where
    generateInterval = do
      size <- stRandomR (minSize, to - from)
      start <- stRandomR (0, to - size - from)
      return (from + start, from + start + size)

roomIn :: RandomGen g => GeneratorParameters -> g -> Space -> (Space, g)
roomIn param g (Space (Coord x1 y1) (Coord x2 y2)) = runState genRoom g
  where
    genRoom = do
      (resX1, resX2) <- generate $ genSubInterval minSize (x1, x2)
      (resY1, resY2) <- generate $ genSubInterval minSize (y1, y2)
      return $ Space (Coord resX1 resY1) (Coord resX2 resY2)
    minSize = minRoomSize param

generateSpaceTree ::
     RandomGen g => GeneratorParameters -> Space -> g -> (BTree Space, g)
generateSpaceTree param s gen =
  fromMaybe (first Leaf $ roomIn param gen s) $ runStateT maybeAns gen
  where
    maybeAns = do
      ((leftSpace, rightSpace), g') <- lift $ splitSpace param s gen
      put g'
      leftTree <- generate $ generateSpaceTree param leftSpace
      rightTree <- generate $ generateSpaceTree param rightSpace
      return $ Branch leftTree rightTree

makeHalls :: (RandomGen g, MonadState g m) => Space -> Space -> m [Hall]
makeHalls (Space from1 to1) (Space from2 to2) = do
  c1@(Coord x1 y1) <- stRandomR (from1, to1)
  c2@(Coord x2 y2) <- stRandomR (from2, to2)
  coinToss <- stRandomR (False, True)
  let middlePoint = if coinToss then Coord x1 y2 else Coord x2 y1
  return [Hall c1 middlePoint, Hall middlePoint c2]

generateHalls
  :: (RandomGen g, MonadState g m)
  => GeneratorParameters
  -> (BTree Space)
  -> m (Space, [Hall])
generateHalls param tree = generateHallsHelper param tree []
  where
    generateHallsHelper :: (RandomGen g, MonadState g m) => GeneratorParameters -> (BTree Space) -> [Hall] -> m (Space, [Hall])
    generateHallsHelper _ (Leaf s) halls = return $ (s, halls)
    generateHallsHelper param (Branch leftT rightT) halls = do
      (leftRoom, halls') <- generateHallsHelper param leftT halls
      (rightRoom, halls'') <- generateHallsHelper param rightT halls'
      newHalls <- makeHalls leftRoom rightRoom
      coinToss <- stRandomR (False, True)
      let returnRoom = if coinToss then leftRoom else rightRoom
      return (returnRoom, newHalls ++ halls'')
      
generateLevel ::
  (RandomGen g, MonadState g m) => GeneratorParameters -> Space -> m ([Space], [Hall])
generateLevel param s = do
  spaceTree <- generate $ generateSpaceTree param s
  (_, halls) <- generateHalls param spaceTree
  return $ (foldr (:) [] spaceTree, halls) 

