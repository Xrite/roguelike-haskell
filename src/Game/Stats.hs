{-# LANGUAGE TemplateHaskell #-}
module Game.Stats where

import Control.Lens

data Stats =
  Stats { _health :: Int, _level :: Int, _attackPower :: Int, _shield :: Int }

makeLenses ''Stats