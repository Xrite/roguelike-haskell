{-# LANGUAGE TemplateHaskell #-}

module Game.TimedEffects where

import           Game.Effect
import           Control.Lens

data TimedEffects = TimedEffects { _effects :: [(Int, Int -> Effect ())] }

makeLenses ''TimedEffects

addEffect :: Int -> (Int -> Effect ()) -> TimedEffects -> TimedEffects
addEffect time effect timedEffects =
  TimedEffects $ (time, effect):_effects timedEffects


tick effects = undefined


