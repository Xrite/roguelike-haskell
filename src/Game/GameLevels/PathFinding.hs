module Game.GameLevels.PathFinding where

import Data.Graph.Inductive hiding (getNode)
import Data.List ((\\))
import Data.Maybe
import Game.GameLevels.GameLevel
import Game.GameLevels.MapCell

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
    neighbours =
      filter isPassable $
        mapMaybe (getNode m) ([(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]] \\ [(x, y)])

findPathInGraph :: MapNode -> MapNode -> MapGraph -> [MapNode] -> Maybe Path
findPathInGraph s t gr exclude = sp (fst s) (fst t) (delNodes (map fst exclude) gr)

findPath ::
  -- | Passability function
  (MapCell -> Bool) ->
  -- | Map to find path in
  Map ->
  -- | Start position
  (Int, Int) ->
  -- | Destination position
  (Int, Int) ->
  -- | Forbidden positions
  [(Int, Int)] ->
  Maybe [(Int, Int)]
findPath passability m s t exclude = do
  sNode <- getNode m s
  tNode <- getNode m t
  path <- findPathInGraph sNode tNode gr (mapMaybe (getNode m) exclude)
  traverse (lab gr) path
  where
    gr = buildGraph passability m
  
getFurtherFrom :: Map -> (MapCell -> Bool) -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
getFurtherFrom m passability s t exclude = furtherNeighbours \\ exclude
  where
    neighbours = let (x, y) = t in [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]
    currentDistance = distance2 s t
    furtherNeighbours = filter isPassable $ filter ((> currentDistance) . distance2 s) neighbours
    isPassable p = inBounds m p && passability (getCell p m)
    distance2 (x1, y1) (x2, y2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2
