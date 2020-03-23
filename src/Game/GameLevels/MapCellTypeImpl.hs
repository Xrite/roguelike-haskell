-- | Module that defines all the cells types that can be used in the game

module Game.GameLevels.MapCellTypeImpl where

import Game.GameLevels.MapCellType

wall :: MapCellType
wall = makeWall '#'

hallGround :: MapCellType
hallGround = makeGround '+'

roomGround :: MapCellType
roomGround = makeGround '.'

tree :: MapCellType
tree = makeWall 'T'

bush :: MapCellType
bush = makeConstCellType '%' True False

ladderDown :: MapCellType
ladderDown = makeGround '>'

ladderUp :: MapCellType
ladderUp = makeGround '<'