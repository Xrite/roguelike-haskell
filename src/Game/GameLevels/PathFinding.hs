module Game.GameLevels.PathFinding where

import Data.Graph.Inductive hiding (getNode)
import Data.List ((\\), minimumBy)
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
  
getFurthestFrom ::
  Map ->
  (MapCell -> Bool) ->
  -- | Start position
  (Int, Int) ->
  -- | Target positions
  [(Int, Int)] ->
  -- | Excluded positions
  [(Int, Int)] ->
  [(Int, Int)]
getFurthestFrom m passability s ts exclude = furthestNeighbours \\ exclude
  where
    neighbours = let (x, y) = s in [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]
    currentDistance = maxDistance s
    furthestNeighbours = filter isPassable $ filter ((> currentDistance) . maxDistance) neighbours
    isPassable p = inBounds m p && passability (getCell p m)
    maxDistance p = maximum $ map (distance2 p) ts
    distance2 (x1, y1) (x2, y2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2

--findNearest :: Map -> (MapCell -> Bool) -> (Int, Int) -> [(Int, Int)] -> 
findNearest ::
  (Num ix, Enum ix) =>
  Map ->
  -- | Passability function
  (MapCell -> Bool) ->
  -- | Start position
  (Int, Int) ->
  -- | Target positions
  [(Int, Int)] ->
  -- | Forbidden positions
  [(Int, Int)] ->
  -- | Shortest path or Nothing if no path exists
  Maybe (ix, [(Int, Int)])
findNearest m passability s ts exclude = do
  sNode <- getNode m s
  tNodes <- mapM (getNode m) ts
  let paths = zip [0 ..] $ mapMaybe (\t -> findPathInGraph sNode t gr excludeNodes) tNodes
  let best = minimumBy (\a b -> compare (length . snd $ a) (length . snd $ b)) paths
  coords <- traverse (lab gr) (snd best)
  return (fst best, coords)
  where
    gr = buildGraph passability m
    excludeNodes = mapMaybe (getNode m) exclude
