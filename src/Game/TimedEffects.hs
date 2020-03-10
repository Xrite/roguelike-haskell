{-# LANGUAGE TemplateHaskell #-}

module Game.TimedEffects where

import           Game.Effect
import           Control.Lens

data TimedEffects = TimedEffects { _effects :: [(Int, Int -> Effect ())] }

makeLenses ''TimedEffects

addEffect :: Int -> (Int -> Effect ()) -> TimedEffects -> TimedEffects
addEffect time effect timedEffects =
  TimedEffects $ (time, effect):_effects timedEffects


tick :: TimedEffects -> TimedEffects
tick (TimedEffects effs) = TimedEffects $ filter ((> 0) . fst) $ map (over _1 (+ (-1))) effs


