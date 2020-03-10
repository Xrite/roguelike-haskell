{-# LANGUAGE TemplateHaskell #-}
module Game.Stats where

import Control.Lens (makeLenses)

data Stats =
  Stats { _health :: Int, _attackPower :: Int, _shield :: Int, _level :: Int }
makeLenses ''Stats