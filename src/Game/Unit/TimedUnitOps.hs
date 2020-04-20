{-# LANGUAGE TemplateHaskell #-}

module Game.Unit.TimedUnitOps
  ( TimedUnitOps
  , empty
  , addUnitOp
  , tick
  , composeUnitOp
  )
where

import           Game.Modifiers.UnitOp
import           Control.Lens

data TimedUnitOps = TimedUnitOps { _modifiers :: [(Int, Int -> UnitOp ())] }

makeLenses ''TimedUnitOps

empty :: TimedUnitOps
empty = TimedUnitOps []

addUnitOp :: Int -> (Int -> UnitOp ()) -> TimedUnitOps -> TimedUnitOps
addUnitOp time modifier timedUnitOps
  | time > 0  = TimedUnitOps $ (time, modifier) : _modifiers timedUnitOps
  | otherwise = timedUnitOps

tick :: TimedUnitOps -> TimedUnitOps
tick (TimedUnitOps effs) =
  TimedUnitOps $ filter ((> 0) . fst) $ map (over _1 (+ (-1))) effs

composeUnitOp :: TimedUnitOps -> UnitOp ()
composeUnitOp = mapM_ (\(i, eff) -> eff i) . _modifiers
  