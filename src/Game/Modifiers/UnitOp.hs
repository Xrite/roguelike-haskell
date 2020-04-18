{-# LANGUAGE DeriveFunctor #-}

module Game.Modifiers.UnitOp
  ( UnitOp
  , UnitOpDSL(..)
  , getStats
  , setStats
  , modifyStats
  , setTimedUnitOp
  , setCoord
  , setEffect
  )
where

import           Game.Unit.Stats
import           Control.Monad.Free
import           Game.Modifiers.EffectAtom

data UnitOpDSL a = GetStats (Maybe Stats -> a)
                                 | SetStats Stats a
                                 | ModifyStats (Stats -> Stats) a
                                 | SetTimedUnitOp Int (Int -> UnitOp ()) a
                                 | MoveTo (Int, Int) a
                                 | ApplyEffect EffectAtom a
                  deriving (Functor)

type UnitOp a = Free UnitOpDSL a 

getStats :: UnitOp (Maybe Stats)
getStats = liftF $ GetStats id

setStats :: Stats -> UnitOp ()
setStats stats = Free $ SetStats stats (Pure ())

modifyStats :: (Stats -> Stats) -> UnitOp ()
modifyStats f = Free $ ModifyStats f (Pure ())

setTimedUnitOp :: Int -> (Int -> UnitOp ()) -> UnitOp ()
setTimedUnitOp time modifier = Free $ SetTimedUnitOp time modifier (Pure ())

setCoord :: (Int, Int) -> UnitOp ()
setCoord coord = Free $ MoveTo coord $ Pure ()

setEffect :: EffectAtom -> UnitOp ()
setEffect atom = Free $ ApplyEffect atom (Pure ())
