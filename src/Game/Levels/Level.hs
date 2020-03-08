module Game.Levels.Level
  ( getCell
  , makeWall
  , makeGround
  , makeCell
  ) where

import Game.Environment
import Control.Lens ((^.))
import Data.Array.IArray
--import Data.Array.ST
--import Control.Monad.ST

--getCellST :: (Int, Int) -> MapST s -> ST s MapCell
--getCellST i map = readArray (map ^. cellsST) i

getCell :: (Int, Int) -> Map -> MapCell
getCell i mp = (mp ^. cells) ! i

makeLevel :: Map -> Level
makeLevel = Level

makeConstCellType :: Char -> Bool -> Bool -> MapCellType
makeConstCellType render' passable' transparent' = MapCellType { _render = render', _transparent = const transparent', _passable = const passable' }

makeWall :: Char -> MapCellType
makeWall render' = makeConstCellType render' False False

makeGround :: Char -> MapCellType
makeGround render' = makeConstCellType render' True False

makeCell :: MapCellType -> [Item] -> MapCell
makeCell = MapCell