{-# LANGUAGE TemplateHaskell #-}

-- | Module responsible for working with the map cell's type

module Game.GameLevels.MapCellType
  ( MapCellType (..)
  , makeCellType
  , makeConstCellType
  , makeGround
  , makeWall
  , cellRender
  , passable
  , transparent
  , interaction
  , onStep
  ) where

import Game.Unit.Stats
import Control.Lens (makeLenses)
import Game.Modifiers.UnitOp

-- | Describes properties of a cell on the map
data MapCellType pos = MapCellType
  { -- | how the cell is going to be drawn
    _cellRender :: Char
    -- | decides whether a unit can pass through the cell
  , _passable :: Stats -> Bool
    -- | decides whether a unit can see through the cell
  , _transparent :: Stats -> Bool
    -- | Cells can let player do weird things like go to the next level or perhaps open a shop.
    
--     For lack of better option I'll make it "UnitOp ()", but
--     it probably should be something more permissive, at least use GameIO 
  , _interaction :: UnitOp pos ()
    -- | Cells can do things then a unit steps on them (fire applies burn modifier etc.).
      
    -- Also cells should be able to modify themselves (MapCell -> CellState), but we'll leave it to later versions. 
  , _onStep :: UnitOp pos ()
  }
makeLenses ''MapCellType

makeCellType :: Char -> (Stats -> Bool) -> (Stats -> Bool) -> UnitOp pos () -> UnitOp pos () -> MapCellType pos
makeCellType = MapCellType

makeConstCellType :: Char -> Bool -> Bool -> MapCellType pos
makeConstCellType render' passable' transparent' =
  MapCellType
    { _cellRender = render'
    , _transparent = const transparent'
    , _passable = const passable'
    , _interaction = return ()
    , _onStep = return ()
    }

-- | Create non-transparent non-passable cell type
makeWall :: Char -> MapCellType pos 
makeWall render' = makeConstCellType render' False False

-- | Create non-transparent passable cell type
makeGround :: Char -> MapCellType pos
makeGround render' = makeConstCellType render' True True
