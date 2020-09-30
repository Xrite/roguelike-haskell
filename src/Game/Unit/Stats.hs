{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Module that represents characteristics of the unit such as health and attack power
module Game.Unit.Stats where

import Control.Lens (makeLenses)
import GHC.Generics (Generic)

data Stats
  = Stats {_health :: Int, _attackPower :: Int, _shield :: Int, _level :: Int}
  deriving (Generic, Eq)

makeLenses ''Stats

defaultStats :: Stats
defaultStats =
  Stats
    { _health = 10,
      _attackPower = 10,
      _shield = 10,
      _level = 1
    }

makeStats ::
  -- | health
  Int ->
  -- | attack power
  Int ->
  -- | shield
  Int ->
  -- | level
  Int ->
  Stats
makeStats = Stats
