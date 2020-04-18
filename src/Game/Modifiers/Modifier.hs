{-# LANGUAGE DeriveFunctor #-}

module Game.Modifiers.Modifier
  ( Modifier,
    ModifierDSL (..),
    getStats,
    setStats,
    modifyStats,
    getPosition,
    setPosition,
    modifyPosition,
    setTimedModifier,
    setCoord,
    getPortrait,
    setEffect,
  )
where

import Control.Monad.Free
import Game.Modifiers.EffectAtom
import Game.Unit.Stats

-- | Low level actions to perform with game entities (e.g. units, doors, objects)
data ModifierDSL a
  = GetStats (Maybe Stats -> a)
  | ModifyStats (Stats -> Stats) a
  | GetPosition ((Int, Int) -> a)
  | ModifyPosition ((Int, Int) -> (Int, Int)) a
  | SetTimedModifier Int (Int -> Modifier ()) a
  | MoveTo (Int, Int) a
  | GetPortrait (Char -> a)
  | ApplyEffect EffectAtom a
  deriving (Functor)

type Modifier a = Free ModifierDSL a

getStats :: Modifier (Maybe Stats)
getStats = liftF $ GetStats id

setStats :: Stats -> Modifier ()
setStats stats = modifyStats $ const stats

modifyStats :: (Stats -> Stats) -> Modifier ()
modifyStats f = Free $ ModifyStats f (Pure ())

getPosition :: Modifier (Int, Int)
getPosition = liftF $ GetPosition id

setPosition :: (Int, Int) -> Modifier ()
setPosition pos = modifyPosition $ const pos

modifyPosition :: ((Int, Int) -> (Int, Int)) -> Modifier ()
modifyPosition f = liftF $ ModifyPosition f ()

setTimedModifier :: Int -> (Int -> Modifier ()) -> Modifier ()
setTimedModifier time modifier = Free $ SetTimedModifier time modifier (Pure ())

setCoord :: (Int, Int) -> Modifier ()
setCoord coord = Free $ MoveTo coord $ Pure ()

getPortrait :: Modifier Char
getPortrait = liftF $ GetPortrait id

setEffect :: EffectAtom -> Modifier ()
setEffect atom = Free $ ApplyEffect atom (Pure ())
