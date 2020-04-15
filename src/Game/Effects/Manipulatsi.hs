{-# LANGUAGE DeriveFunctor #-}

module Game.Effects.Manipulatsi
  ( ProizvolnueManipulatsi
  , ProizvolnueManipulatsiDSL(..)
  , getStats
  , setStats
  , modifyStats
  , setAOEEffect
  , setTimedEffect
  , setCoord
  )
where

import           Game.Unit.Stats
import           Control.Monad.Free

-- I want EffectReceiver typeclass so that map cells could do smth like
-- also "burn down then receiving fire dmg"

data ProizvolnueManipulatsiDSL a = GetStats (Maybe Stats -> a)
                 | SetStats Stats a
                 | ModifyStats (Stats -> Stats) a
                 | SetTimedEffect Int (Int -> ProizvolnueManipulatsi ()) a
                 | MoveTo (Int, Int) a
                 {-|
                   Sets effect on everyone in certain range.
                   Strength may depend on distance.
                 -}
                  | AOEEffect Int (Int -> ProizvolnueManipulatsi ()) a
  deriving (Functor)

type ProizvolnueManipulatsi a = Free ProizvolnueManipulatsiDSL a

getStats :: ProizvolnueManipulatsi (Maybe Stats)
getStats = liftF $ GetStats id

setStats :: Stats -> ProizvolnueManipulatsi ()
setStats stats = Free $ SetStats stats (Pure ())

modifyStats :: (Stats -> Stats) -> ProizvolnueManipulatsi ()
modifyStats f = Free $ ModifyStats f (Pure ())

setTimedEffect :: Int -> (Int -> ProizvolnueManipulatsi ()) -> ProizvolnueManipulatsi ()
setTimedEffect time effect = Free $ SetTimedEffect time effect (Pure ())

setAOEEffect :: Int -> (Int -> ProizvolnueManipulatsi ()) -> ProizvolnueManipulatsi ()
setAOEEffect range effect = Free $ AOEEffect range effect (Pure ())

setCoord :: (Int, Int) -> ProizvolnueManipulatsi ()
setCoord coord = Free $ MoveTo coord $ Pure ()
