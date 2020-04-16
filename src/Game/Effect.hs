{-# LANGUAGE DeriveFunctor #-}

module Game.Effect
  ( Effect
  , EffectDSL(..)
  , getStats
  , setStats
  , modifyStats
  , setTimedEffect
  , getPosition
  , setPosition
  , setLevelDepth
  , modifyLevelDepth
  , getLevelDepth
  )
where

import           Game.Unit.Stats
import           Control.Monad.Free

-- I want EffectReceiver typeclass so that map cells could do smth like 
-- also "burn down then receiving fire dmg"

data EffectDSL a = GetStats (Maybe Stats -> a)
                 | ModifyStats (Stats -> Stats) a
                 | SetTimedEffect Int (Int -> Effect ()) a
                 | GetPosition ((Int, Int) -> a)
                 | ModifyPosition ((Int, Int) -> (Int, Int)) a
                 | GetLevelDepth (Int -> a)
                 | ModifyLevelDepth (Int -> Int) a
  deriving (Functor)

type Effect a = Free EffectDSL a

getStats :: Effect (Maybe Stats)
getStats = liftF $ GetStats id

setStats :: Stats -> Effect ()
setStats stats = Free $ ModifyStats (const stats) (Pure ())

modifyStats :: (Stats -> Stats) -> Effect ()
modifyStats f = Free $ ModifyStats f (Pure ())

setTimedEffect :: Int -> (Int -> Effect ()) -> Effect ()
setTimedEffect time effect = Free $ SetTimedEffect time effect (Pure ())

getPosition :: Effect (Int, Int)
getPosition = liftF $ GetPosition id

setPosition :: (Int, Int) -> Effect ()
setPosition coord = Free $ ModifyPosition (const coord) $ Pure ()

setLevelDepth :: Int -> Effect ()
setLevelDepth depth = liftF $ ModifyLevelDepth (const depth) ()

modifyLevelDepth :: (Int -> Int) -> Effect ()
modifyLevelDepth f = liftF $ ModifyLevelDepth f ()

getLevelDepth :: Effect Int
getLevelDepth = liftF $ GetLevelDepth id