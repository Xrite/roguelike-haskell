{-# LANGUAGE TemplateHaskell #-}
module Game.Levels.MapCellType where

import Game.Mob
import Control.Lens (makeLenses)
import Game.IO.GameIO
import Game.Effect

{-|
  Describes properties of a cell on the map
-}
data MapCellType = MapCellType
  { -- | how the cell is going to be drawn
    _cellRender :: Char
    -- | decides whether a mob can pass through the cell
  , _passable :: Mob -> Bool
    -- | decides whether a mob can see through the cell
  , _transparent :: Mob -> Bool
    {-|
      Cells can let player do weird things like go to the next level or perhaps open a shop.

      For lack of better option I'll make it "Effect ()", but
      it probably should be something more permissive, at least use GameIO 
    -}
  , _interact :: Effect ()
    {-|
      Cells can do things then a mob steps on them (fire applies burn effect etc.).
      
      Also cells should be able to modify themselves (MapCell -> CellState), but we'll leave it to later versions. 
    -}
  , _onStep :: Effect ()
  }
makeLenses ''MapCellType

makeConstCellType :: Char -> Bool -> Bool -> MapCellType
makeConstCellType render' passable' transparent' =
  MapCellType
    { _cellRender = render'
    , _transparent = const transparent'
    , _passable = const passable'
    , _interact = return ()
    }

makeWall :: Char -> MapCellType
makeWall render' = makeConstCellType render' False False

makeGround :: Char -> MapCellType
makeGround render' = makeConstCellType render' True False


