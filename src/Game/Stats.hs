{-# LANGUAGE TemplateHaskell #-}
module Game.Stats where

import Control.Lens (makeLenses)

data Stats =
  Stats { _health :: Int, _attackPower :: Int, _shield :: Int }
makeLenses ''Stats

data GameLevelingController = GameLevelingController
  { _experience :: Int
  , _level :: Int
  , _skillPoints :: Int
  -- |How stats are modified when unit gets XP
  , _receiveXP :: Int -> (Stats -> Stats)
  }
makeLenses ''GameLevelingController