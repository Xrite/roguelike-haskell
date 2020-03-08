{-# LANGUAGE DeriveFunctor #-}

module Game.Effect (Effect, getStats, setStats, modifyStats, setTimedEffect) where

import           Control.Monad.Free
import           Game.Stats

data EffectDSL a = GetStats (Stats -> a)
                 | SetStats Stats a
                 | ModifyStats (Stats -> Stats) a
                 | SetTimedEffect Int (Int -> Effect ()) a
  deriving (Functor)

type Effect a = Free EffectDSL a

getStats :: Effect Stats
getStats = liftF $ GetStats id

setStats :: Stats -> Effect ()
setStats stats = Free $ SetStats stats (Pure ())

modifyStats :: (Stats -> Stats) -> Effect ()
modifyStats f = Free $ ModifyStats f (Pure ())

setTimedEffect :: Int -> (Int -> Effect ()) -> Effect ()
setTimedEffect time effect = Free $ SetTimedEffect time effect (Pure ())
