{-# LANGUAGE TemplateHaskell #-}

module Game.Unit.TimedModifiers
  ( TimedModifiers
  , empty
  , addModifier
  , tick
  , composeModifier
  )
where

import           Game.Modifiers.Modifier
import           Control.Lens

data TimedModifiers = TimedModifiers { _modifiers :: [(Int, Int -> Modifier ())] }

makeLenses ''TimedModifiers

empty :: TimedModifiers
empty = TimedModifiers []

addModifier :: Int -> (Int -> Modifier ()) -> TimedModifiers -> TimedModifiers
addModifier time modifier timedModifiers
  | time > 0  = TimedModifiers $ (time, modifier) : _modifiers timedModifiers
  | otherwise = timedModifiers

tick :: TimedModifiers -> TimedModifiers
tick (TimedModifiers effs) =
  TimedModifiers $ filter ((> 0) . fst) $ map (over _1 (+ (-1))) effs

composeModifier :: TimedModifiers -> Modifier ()
composeModifier = mapM_ (\(i, eff) -> eff i) . _modifiers
