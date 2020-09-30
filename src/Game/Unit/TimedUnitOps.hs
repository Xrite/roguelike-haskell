{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Game.Unit.TimedUnitOps
  ( TimedUnitOps
  , empty
  , addUnitOp
  , tick
  , composeUnitOp
  )
where

import           Control.Lens
import           GHC.Generics (Generic)
import Game.Modifiers.EffectDesc (EffectDesc)

newtype TimedUnitOps = TimedUnitOps{_modifiers :: [[EffectDesc]]} deriving (Generic, Eq)

makeLenses ''TimedUnitOps

empty :: TimedUnitOps
empty = TimedUnitOps []

addUnitOp :: Int -> (Int -> EffectDesc) -> TimedUnitOps -> TimedUnitOps
addUnitOp time modifier timedUnitOps
  | time > 0  = TimedUnitOps $ map modifier [1 .. time] : _modifiers timedUnitOps
  | otherwise = timedUnitOps

tick :: TimedUnitOps -> TimedUnitOps
tick (TimedUnitOps effs) = TimedUnitOps $ filter (not . null) $ map tail effs

composeUnitOp :: TimedUnitOps -> EffectDesc
composeUnitOp = mapM_ head . _modifiers
