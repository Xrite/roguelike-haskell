{-# LANGUAGE TemplateHaskell, Rank2Types #-}
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
      Cells can do weird things like transport player to the next level or perhaps open a shop.

      It can modify game state in any way. For lack of better option I'll make it "GameIO (Effect ())", but
      actually it should be in the most general monad possible.
    -}
  , _interact :: GameIO (Effect ())
  }
makeLenses ''MapCellType

makeConstCellType :: Char -> Bool -> Bool -> MapCellType
makeConstCellType render' passable' transparent' =
  MapCellType
    { _cellRender = render'
    , _transparent = const transparent'
    , _passable = const passable'
    , _interact = pureGameIO $ return () -- TODO replace with something better
    }

makeWall :: Char -> MapCellType
makeWall render' = makeConstCellType render' False False

makeGround :: Char -> MapCellType
makeGround render' = makeConstCellType render' True False


