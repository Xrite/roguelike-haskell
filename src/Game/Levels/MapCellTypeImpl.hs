module Game.Levels.MapCellTypeImpl where

import Game.Levels.MapCellType

wall :: MapCellType
wall = makeWall '#'

ground :: MapCellType
ground = makeGround '.'

tree :: MapCellType
tree = makeWall 'T'

bush :: MapCellType
bush = makeConstCellType '%' True False

ladderDown :: MapCellType
ladderDown = makeGround '>'

ladderUp :: MapCellType
ladderUp = makeGround '<'