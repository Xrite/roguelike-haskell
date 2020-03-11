{-# LANGUAGE TemplateHaskell #-}
module Game.Unit.Stats where

import Control.Lens (makeLenses)

data Stats =
  Stats { _health :: Int, _attackPower :: Int, _shield :: Int, _level :: Int }
makeLenses ''Stats