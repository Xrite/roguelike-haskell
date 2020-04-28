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

cellTypeByKey :: MapCellTypeKey -> MapCellType
cellTypeByKey '#' = wall
cellTypeByKey '+' = hallGround
cellTypeByKey '.' = roomGround
cellTypeByKey 'T' = tree
cellTypeByKey '%' = bush
cellTypeByKey '>' = ladderDown
cellTypeByKey '<' = ladderUp
cellTypeByKey _ = error "cell type does not exist"