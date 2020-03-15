{-# LANGUAGE DeriveFunctor #-}

module Game.Effect
  ( Effect
  , EffectDSL(..)
  , getStats
  , setStats
  , modifyStats
--    , setAOEEffect
  , setTimedEffect
  , setCoord
  )
where

import           Game.Unit.Stats
import           Control.Monad.Free

-- I want EffectReceiver typeclass so that map cells could do smth like 
-- also "burn down then receiving fire dmg"

data EffectDSL a = GetStats (Stats -> a)
                 | SetStats Stats a
                 | ModifyStats (Stats -> Stats) a
                 | SetTimedEffect Int (Int -> Effect ()) a
                 | MoveTo (Int, Int) a
--                 {-|
--                   Sets effect on everyone in certain range.
--                   Strength may depend on distance.
--                 -}
--                 | AOEEffect Int (Int -> Effect ()) a
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

setCoord :: (Int, Int) -> Effect ()
setCoord coord = Free $ MoveTo coord $ Pure ()

--setAOEEffect :: Int -> (Int -> Effect ()) -> Effect ()
--setAOEEffect range effect = Free $ AOEEffect range effect (Pure ())
