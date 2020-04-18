module Game.Modifiers.UnitOpFactory
  ( UnitOpFactory,
    makeUnitOpFactory,
    buildUnitOp,
  )
where

import Control.Monad.Free (Free (..))
import Data.Map
import Game.Modifiers.EffectDesc
import Game.Modifiers.UnitOp
import Game.Unit.Unit

newtype UnitOpFactory = UnitOpFactory (Map UnitOpKey (UnitOp ()))

makeUnitOpFactory :: Map UnitOpKey (UnitOp ()) -> UnitOpFactory
makeUnitOpFactory = UnitOpFactory

buildUnitOp :: Unit u => UnitOpFactory -> u -> EffectDesc -> UnitOp ()
buildUnitOp factory u (Free (Atom atom next)) = setEffect atom >> buildUnitOp factory u next
buildUnitOp factory@(UnitOpFactory mp) u (Free (TypicalUnitOp key next)) = mp ! key >> buildUnitOp factory u next
buildUnitOp _ u (Pure ()) = Pure ()