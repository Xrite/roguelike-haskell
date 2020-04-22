{-# LANGUAGE DeriveFunctor #-}

module Game.Modifiers.UnitOp
  ( UnitOp,
    UnitOpDSL (..),
    getStats,
    setStats,
    modifyStats,
    getPosition,
    setPosition,
    modifyPosition,
    setTimedUnitOp,
    setCoord,
    getPortrait,
    setEffect,
    getConfusion,
    tickTimedEffects
  )
where

import Control.Monad.Free
import Game.Modifiers.EffectAtom
import Game.Unit.Stats

-- | Low level actions to perform with game entities (e.g. units, doors, objects)
data UnitOpDSL a
  = GetStats (Maybe Stats -> a)
  | ModifyStats (Stats -> Stats) a
  | GetPosition ((Int, Int) -> a)
  | ModifyPosition ((Int, Int) -> (Int, Int)) a
  | SetTimedUnitOp Int (Int -> UnitOp ()) a
  | MoveTo (Int, Int) a
  | GetPortrait (Char -> a)
  | ApplyEffect EffectAtom a
  | GetConfusion (Bool -> a)
  | TickTimedEffects a
  deriving (Functor)

type UnitOp a = Free UnitOpDSL a

getStats :: UnitOp (Maybe Stats)
getStats = liftF $ GetStats id

setStats :: Stats -> UnitOp ()
setStats stats = modifyStats $ const stats

modifyStats :: (Stats -> Stats) -> UnitOp ()
modifyStats f = Free $ ModifyStats f (Pure ())

getPosition :: UnitOp (Int, Int)
getPosition = liftF $ GetPosition id

setPosition :: (Int, Int) -> UnitOp ()
setPosition pos = modifyPosition $ const pos

modifyPosition :: ((Int, Int) -> (Int, Int)) -> UnitOp ()
modifyPosition f = liftF $ ModifyPosition f ()

setTimedUnitOp :: Int -> (Int -> UnitOp ()) -> UnitOp ()
setTimedUnitOp time modifier = Free $ SetTimedUnitOp time modifier (Pure ())

setCoord :: (Int, Int) -> UnitOp ()
setCoord coord = Free $ MoveTo coord $ Pure ()

getPortrait :: UnitOp Char
getPortrait = liftF $ GetPortrait id

setEffect :: EffectAtom -> UnitOp ()
setEffect atom = Free $ ApplyEffect atom (Pure ())

getConfusion :: UnitOp Bool
getConfusion = liftF $ GetConfusion id

tickTimedEffects :: UnitOp ()
tickTimedEffects = Free $ TickTimedEffects (Pure ())