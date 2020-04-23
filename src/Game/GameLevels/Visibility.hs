module Game.GameLevels.Visibility where

import Data.Graph.Inductive hiding (getNode)
import Data.Maybe
import Data.Ratio
import Data.List
import Game.GameLevels.GameLevel
import Game.GameLevels.MapCell
import Game.GameLevels.PathFinding

canSeeSlow :: Map -> (MapCell -> Bool) -> (Int, Int) -> (Int, Int) -> Bool
canSeeSlow m visibility (ax, ay) (bx, by) = fromMaybe False $ do
  sNode <- getNode m (ax, ay)
  tNode <- getNode m (bx, by)
  _ <- sp (fst sNode) (fst tNode) $ (mkGraph allNodes allEdges :: Gr (Int, Int) Int)
  return True
  where
    (ux, uy) = (bx - ax, by - ay)
    vec (x1, y1) (x2, y2) = x1 * y2 - x2 * y1
    dist (x, y) = if ux == 0 && uy == 0 then 0 % 1 else (vec (x - ax, y - ay) (ux, uy)) ^ 2 % (ux ^ 2 + uy ^ 2)
    isClose (x, y) = dist (x, y) <= 1 % 2
    ((xFrom, yFrom), (xTo, yTo)) = getMapSize m
    allNodes = mapMaybe (getNode m) [(x, y) | x <- [xFrom .. xTo], y <- [yFrom .. yTo], isClose (x, y)]
    isVisible (index, (x, y)) = inBounds m (x, y) && visibility (getCell (x, y) m)
    allEdges = concatMap (buildEdgesToNeighbours m isVisible) allNodes

canSeeFully :: Map -> (MapCell -> Bool) -> (Int, Int) -> (Int, Int) -> Bool
canSeeFully m visibility (x1, y1) (x2, y2) = check
  where
    a = y1 - y2
    b = x2 - x1
    c = x1 * y2 - x2 * y1
    xPts = if b == 0 then [] else [(x, round $ - fromIntegral (a * x + c) / fromIntegral b) | x <- [(min x1 x2) .. (max x1 x2)]]
    yPts = if a == 0 then [] else [(round $ - fromIntegral (b * y + c) / fromIntegral a, y) | y <- [(min y1 y2) .. (max y1 y2)]]
    isVisible (x, y) = inBounds m (x, y) && visibility (getCell (x, y) m)
    check = all isVisible $ xPts ++ yPts

canSee :: Map -> (MapCell -> Bool) -> (Int, Int) -> (Int, Int) -> Bool
canSee m visibility s (x, y) = if inBounds m (x, y) && visibility (getCell (x, y) m)
  then canSeeFully m visibility s (x, y)
  else nonTransparentCase
  where
    nonTransparentCase = any (canSeeFully m visibility s) [(x + dx, y + dy) | (dx, dy) <- [(1, 0), (-1, 0), (0, 1), (0, -1)]]

-- | Return all visible positions from given position.
visiblePositions :: Map -> (MapCell -> Bool) -> (Int, Int) -> [(Int, Int)]
visiblePositions m visibility pos = filter (canSee m visibility pos) allPositions
  where
    ((xFrom, yFrom), (xTo, yTo)) = getMapSize m
    allPositions = [(x, y) | x <- [xFrom .. xTo], y <- [yFrom .. yTo]]
