module Game.GameLevels.PathFinding where

import Data.Maybe
import Data.List ((\\))
import Data.Graph.Inductive hiding (getNode)
import Game.GameLevels.GameLevel
import Game.GameLevels.MapCell
import Game.GameLevels.MapCellType

type MapGraph = Gr (Int, Int) Int

type MapNode = LNode (Int, Int)

type MapEdge = LEdge Int

getNode :: Map -> (Int, Int) -> Maybe MapNode
getNode m (x, y) =
  if inBounds m (x, y)
    then Just (index, (x, y))
    else Nothing
  where
    ((xFrom, yFrom), (xTo, yTo)) = getMapSize m
    index = x - xFrom + (y - yFrom) * (xTo - xFrom + 1)

buildGraph ::
  -- | Passability function
  (MapCell -> Bool) ->
  Map ->
  MapGraph
buildGraph passability m = mkGraph allNodes allEdges
  where
    ((xFrom, yFrom), (xTo, yTo)) = getMapSize m
    allNodes = mapMaybe (getNode m) [(x, y) | x <- [xFrom .. xTo], y <- [yFrom .. yTo]]
    isPassable (index, (x, y)) = inBounds m (x, y) && passability (getCell (x, y) m)
    allEdges = concatMap (buildEdgesToNeighbours m isPassable) allNodes

buildEdgesToNeighbours :: Map -> (MapNode -> Bool) -> MapNode -> [MapEdge]
buildEdgesToNeighbours m isPassable u@(index, (x, y)) = [(fst u, fst v, 1) | v <- neighbours]
  where
    neighbours = filter isPassable $ mapMaybe (getNode m) ([(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]] \\ [(x, y)])

findPath :: MapNode -> MapNode -> MapGraph -> [MapNode] -> Maybe Path
findPath s t gr exclude = sp (fst s) (fst t) (delNodes (map fst exclude) gr)