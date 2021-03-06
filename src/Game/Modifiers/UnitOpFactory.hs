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
import Data.Maybe (fromMaybe)

-- |Describes default 'UnitOp's that can be used in 'EffectDesc'.
newtype UnitOpFactory = UnitOpFactory (Map UnitOpKey (UnitOp ()))

-- |Creates a 'UnitOpFactory' from a map.
makeUnitOpFactory :: Map UnitOpKey (UnitOp ()) -> UnitOpFactory 
makeUnitOpFactory = UnitOpFactory

-- |Builds a 'UnitOp' substituting references to default 'UnitOp's using provided 'UnitOpFactory'.
buildUnitOp :: UnitOpFactory -> EffectDesc -> UnitOp ()
buildUnitOp factory (Free (Atom atom next)) = setEffect atom >> buildUnitOp factory next
buildUnitOp factory@(UnitOpFactory mp) (Free (TypicalUnitOp key next)) = fromMaybe (error $ "effect not found: " ++ key) (mp !? key) >> buildUnitOp factory next
buildUnitOp _ (Pure ()) = Pure ()