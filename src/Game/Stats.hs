module Game.Stats (Stats, health, level, attackPower, shield) where

data Stats =
  Stats { _health :: Int, _level :: Int, _attackPower :: Int, _shield :: Int }

health = _health

level = _level

attackPower = _attackPower

shield = _shield





