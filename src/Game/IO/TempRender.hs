-- A place to store debug and temporary renders until IO is done
module Game.IO.TempRender where

import Control.Lens ((^.))
import Game.GameLevels.GameLevel
import Game.GameLevels.MapCell (renderCell)

renderLevel :: GameLevel -> [String]
renderLevel lvl =
  [[renderCell $ getCell (x, y) mp | x <- [xFrom .. xTo]] | y <- [yFrom .. yTo]]
  where
    mp = lvl ^. lvlMap
    ((xFrom, yFrom), (xTo, yTo)) = getMapSize mp

