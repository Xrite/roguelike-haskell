{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}

module Game.GameLevels.Generation.BSPGen where

import Control.Lens
import System.Random
import Data.Tuple (swap)

data Coord =
  Coord
    { _xCoord :: Int
    , _yCoord :: Int
    }

makeLenses ''Coord

data Room =
  Room
    { _fromCorner :: Coord
    , _toCorner :: Coord
    }
makeLenses ''Room

data Space =
  Space
    { _fromCoord :: Coord
    , _toCoord :: Coord
    }
makeLenses ''Space


data BTree a
  = Branch
      { _leftSon :: BTree a
      , _rightSon :: BTree a
      }
  | Leaf
      { _leafValue :: a
      }
  deriving (Eq, Show, Functor)

makeLenses ''BTree

data GeneratorParameters =
  GeneratorParameters
    { minRoomSize :: Int
    , sizesRatioFix :: Float
    }

spaceSize :: Space -> Coord
spaceSize (Space (Coord x1 y1) (Coord x2 y2)) = Coord (x2 - x1) (y2 - y1)

spaceSizeX :: Space -> Int
spaceSizeX s | (Coord xDiff yDiff) <- spaceSize s = xDiff

spaceSizeY :: Space -> Int
spaceSizeY s | (Coord xDiff yDiff) <- spaceSize s = yDiff

splitSpace :: RandomGen g => g -> GeneratorParameters -> Space -> Maybe (Space, Space, g)
splitSpace gen param s@(Space from to) = undefined
  where
    ratio = fromIntegral (spaceSizeX s) / fromIntegral (spaceSizeY s)
    splitX
      | ratio > sizesRatioFix param = (True, gen)
      | ratio > sizesRatioFix param = (False, gen)
      | otherwise = randomR (False, True) gen


generateRoomTree :: RandomGen g => g -> Coord -> Coord -> (BTree Room, g)
generateRoomTree gen from to = undefined
  where
    (left, g') = undefined

