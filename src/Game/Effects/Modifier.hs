{-# LANGUAGE DeriveFunctor #-}

module Game.Effects.Modifier
  ( Modifier
  , ModifierDSL(..)
  , getStats
  , setStats
  , modifyStats
  , setTimedEffect
  , setCoord
  )
where

import           Game.Unit.Stats
import           Control.Monad.Free
import           Game.Effects.EffectAtom

-- I want EffectReceiver typeclass so that map cells could do smth like
-- also "burn down then receiving fire dmg"

data ModifierDSL a = GetStats (Maybe Stats -> a)
                                 | SetStats Stats a
                                 | ModifyStats (Stats -> Stats) a
                                 | SetTimedEffect Int (Int -> Modifier ()) a
                                 | MoveTo (Int, Int) a
                                 | ApplyEffect EffectAtom a
                  deriving (Functor)

type Modifier a = Free ModifierDSL a

getStats :: Modifier (Maybe Stats)
getStats = liftF $ GetStats id

setStats :: Stats -> Modifier ()
setStats stats = Free $ SetStats stats (Pure ())

modifyStats :: (Stats -> Stats) -> Modifier ()
modifyStats f = Free $ ModifyStats f (Pure ())

setTimedEffect :: Int -> (Int -> Modifier ()) -> Modifier ()
setTimedEffect time effect = Free $ SetTimedEffect time effect (Pure ())

setCoord :: (Int, Int) -> Modifier ()
setCoord coord = Free $ MoveTo coord $ Pure ()
