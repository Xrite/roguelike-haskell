module Game.Modifiers.UnitOpFactory
  ( UnitOpFactory
  , makeUnitOpFactory
  , buildUnitOp
  )
where

import Game.Modifiers.EffectDesc
import Game.Modifiers.UnitOp
import Data.Map
import Control.Monad.Free (Free (..))

newtype UnitOpFactory = UnitOpFactory (Map UnitOpKey (UnitOp ()))

makeUnitOpFactory :: Map UnitOpKey (UnitOp ()) -> UnitOpFactory
makeUnitOpFactory = UnitOpFactory

buildUnitOp :: UnitOpFactory -> EffectDesc -> UnitOp ()
buildUnitOp factory (Free (Atom atom next)) = setEffect atom >> buildUnitOp factory next
buildUnitOp factory@(UnitOpFactory mp) (Free (TypicalUnitOp key next)) = mp ! key >> buildUnitOp factory next
buildUnitOp _ (Pure ()) = Pure ()
