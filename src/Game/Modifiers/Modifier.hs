{-# LANGUAGE DeriveFunctor #-}

module Game.Modifiers.Modifier
  ( Modifier
  , ModifierDSL(..)
  , getStats
  , setStats
  , modifyStats
  , setTimedModifier
  , setCoord
  )
where

import           Game.Unit.Stats
import           Control.Monad.Free
import           Game.Modifiers.EffectAtom

-- I want ModifierReceiver typeclass so that map cells could do smth like
-- also "burn down then receiving fire dmg"

data ModifierDSL a = GetStats (Maybe Stats -> a)
                                 | SetStats Stats a
                                 | ModifyStats (Stats -> Stats) a
                                 | SetTimedModifier Int (Int -> Modifier ()) a
                                 | MoveTo (Int, Int) a
                                 | ApplyModifier EffectAtom a
                  deriving (Functor)

type Modifier a = Free ModifierDSL a

getStats :: Modifier (Maybe Stats)
getStats = liftF $ GetStats id

setStats :: Stats -> Modifier ()
setStats stats = Free $ SetStats stats (Pure ())

modifyStats :: (Stats -> Stats) -> Modifier ()
modifyStats f = Free $ ModifyStats f (Pure ())

setTimedModifier :: Int -> (Int -> Modifier ()) -> Modifier ()
setTimedModifier time modifier = Free $ SetTimedModifier time modifier (Pure ())

setCoord :: (Int, Int) -> Modifier ()
setCoord coord = Free $ MoveTo coord $ Pure ()
