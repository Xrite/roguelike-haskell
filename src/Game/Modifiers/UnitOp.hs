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
    tickTimedEffects,
  )
where

import Control.Monad.Free
import Game.Modifiers.EffectAtom
import Game.Modifiers.EffectDesc (EffectDesc)
import Game.Unit.Stats

-- | Low level actions to perform with game entities (e.g. units, doors, objects)
data UnitOpDSL pos a
  = GetStats (Maybe Stats -> a)
  | ModifyStats (Stats -> Stats) a
  | GetPosition (pos -> a)
  | ModifyPosition (pos -> pos) a
  | SetTimedUnitOp Int (Int -> EffectDesc) a
  | MoveTo pos a
  | GetPortrait (Char -> a)
  | ApplyEffect EffectAtom a
  | GetConfusion (Bool -> a)
  | TickTimedEffects a
  deriving (Functor)

type UnitOp pos a = Free (UnitOpDSL pos) a

getStats :: UnitOp pos (Maybe Stats)
getStats = liftF $ GetStats id

setStats :: Stats -> UnitOp pos ()
setStats stats = modifyStats $ const stats

modifyStats :: (Stats -> Stats) -> UnitOp pos ()
modifyStats f = Free $ ModifyStats f (Pure ())

getPosition :: UnitOp pos pos
getPosition = liftF $ GetPosition id

setPosition :: pos -> UnitOp pos ()
setPosition pos = modifyPosition $ const pos

modifyPosition :: (pos -> pos) -> UnitOp pos ()
modifyPosition f = liftF $ ModifyPosition f ()

setTimedUnitOp :: Int -> (Int -> EffectDesc) -> UnitOp pos ()
setTimedUnitOp time modifier = Free $ SetTimedUnitOp time modifier (Pure ())

setCoord :: pos -> UnitOp pos ()
setCoord coord = Free $ MoveTo coord $ Pure ()

getPortrait :: UnitOp pos Char
getPortrait = liftF $ GetPortrait id

setEffect :: EffectAtom -> UnitOp pos ()
setEffect atom = Free $ ApplyEffect atom (Pure ())

getConfusion :: UnitOp pos Bool
getConfusion = liftF $ GetConfusion id

tickTimedEffects :: UnitOp pos ()
tickTimedEffects = Free $ TickTimedEffects (Pure ())
