module Game.GameLevels.MapCellTypeImpl where

import Game.GameLevels.MapCellType

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