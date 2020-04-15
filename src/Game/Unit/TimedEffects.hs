{-# LANGUAGE TemplateHaskell #-}

module Game.Unit.TimedEffects
  ( TimedEffects
  , empty
  , addEffect
  , tick
  , composeEffect
  )
where

import           Game.Effects.Manipulatsi
import           Control.Lens

data TimedEffects = TimedEffects { _effects :: [(Int, Int -> ProizvolnueManipulatsi ())] }

makeLenses ''TimedEffects

empty :: TimedEffects
empty = TimedEffects []

addEffect :: Int -> (Int -> ProizvolnueManipulatsi ()) -> TimedEffects -> TimedEffects
addEffect time effect timedEffects
  | time > 0  = TimedEffects $ (time, effect) : _effects timedEffects
  | otherwise = timedEffects

tick :: TimedEffects -> TimedEffects
tick (TimedEffects effs) =
  TimedEffects $ filter ((> 0) . fst) $ map (over _1 (+ (-1))) effs

composeEffect :: TimedEffects -> ProizvolnueManipulatsi ()
composeEffect = mapM_ (\(i, eff) -> eff i) . _effects
