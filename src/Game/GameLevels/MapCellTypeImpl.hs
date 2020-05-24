-- | Module that defines all the cells types that can be used in the game

module Game.GameLevels.MapCellTypeImpl where

import Game.GameLevels.MapCellType

type MapCellTypeKey = Char

wallKey :: MapCellTypeKey
wallKey = '#'
hallGroundKey :: MapCellTypeKey
hallGroundKey = '+'
roomGroundKey :: MapCellTypeKey
roomGroundKey = '.'
treeKey :: MapCellTypeKey
treeKey = 'T'
bushKey :: MapCellTypeKey
bushKey = '%'
ladderDownKey :: MapCellTypeKey
ladderDownKey = '>'
ladderUpKey :: MapCellTypeKey
ladderUpKey = '<'

wall :: MapCellType pos
wall = makeWall '#'

hallGround :: MapCellType pos
hallGround = makeGround '+'

roomGround :: MapCellType pos
roomGround = makeGround '.'

tree :: MapCellType pos
tree = makeWall 'T'

bush :: MapCellType pos
bush = makeConstCellType '%' True False

ladderDown :: MapCellType pos
ladderDown = makeGround '>'

ladderUp :: MapCellType pos
ladderUp = makeGround '<'

cellTypeByKey :: MapCellTypeKey -> MapCellType pos
cellTypeByKey '#' = wall
cellTypeByKey '+' = hallGround
cellTypeByKey '.' = roomGround
cellTypeByKey 'T' = tree
cellTypeByKey '%' = bush
cellTypeByKey '>' = ladderDown
cellTypeByKey '<' = ladderUp
cellTypeByKey _ = error "cell type does not exist"