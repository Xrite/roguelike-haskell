module Game.Levels.MapCellTypes where

import Game.Levels.Level
import Game.Environment

wall :: MapCellType
wall = makeWall '#'

ground :: MapCellType
ground = makeGround '.'

tree :: MapCellType
tree = makeWall 'T'

bush :: MapCellType
bush = makeConstCellType '%' True False