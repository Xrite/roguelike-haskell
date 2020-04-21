module Game.GameLevels.Visibility where

import Data.Graph.Inductive hiding (getNode)
import Data.Maybe
import Data.Ratio
import Game.GameLevels.GameLevel
import Game.GameLevels.MapCell
import Game.GameLevels.PathFinding

canSee :: Map -> (MapCell -> Bool) -> (Int, Int) -> (Int, Int) -> Bool
canSee m visibility (ax, ay) (bx, by) = fromMaybe False $ do
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
