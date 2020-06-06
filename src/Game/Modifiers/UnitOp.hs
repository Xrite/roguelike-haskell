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
import Game.Position

-- | Low level actions to perform with game entities (e.g. units, doors, objects)
data UnitOpDSL a
  = GetStats (Maybe Stats -> a)
  | ModifyStats (Stats -> Stats) a
  | GetPosition (Position -> a)
  | ModifyPosition (Position -> Position) a
  | SetTimedUnitOp Int (Int -> EffectDesc) a
  | MoveTo Position a
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

getPosition :: UnitOp Position
getPosition = liftF $ GetPosition id

setPosition :: Position -> UnitOp ()
setPosition pos = modifyPosition $ const pos

modifyPosition :: (Position -> Position) -> UnitOp ()
modifyPosition f = liftF $ ModifyPosition f ()

setTimedUnitOp :: Int -> (Int -> EffectDesc) -> UnitOp ()
setTimedUnitOp time modifier = Free $ SetTimedUnitOp time modifier (Pure ())

setCoord :: Position -> UnitOp ()
setCoord coord = Free $ MoveTo coord $ Pure ()

getPortrait :: UnitOp Char
getPortrait = liftF $ GetPortrait id

setEffect :: EffectAtom -> UnitOp ()
setEffect atom = Free $ ApplyEffect atom (Pure ())

getConfusion :: UnitOp Bool
getConfusion = liftF $ GetConfusion id

tickTimedEffects :: UnitOp ()
tickTimedEffects = Free $ TickTimedEffects (Pure ())
