{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Module that represents characteristics of the unit such as health and attack power

module Game.Unit.Stats where

import Control.Lens (makeLenses)
import GHC.Generics (Generic)
 
data Stats =
  Stats { _health :: Int, _attackPower :: Int, _shield :: Int, _level :: Int }
  deriving (Generic, Eq)
makeLenses ''Stats