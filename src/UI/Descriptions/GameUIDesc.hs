{-# LANGUAGE TemplateHaskell #-}

module UI.Descriptions.GameUIDesc where

import           Control.Monad.State
import           Control.Lens

data UIDesc action = GameUIDesc { __getMap :: Map
                                , __getLog :: Log
                                , __getStats :: Stats
                                , __itemMenu :: ItemMenu action
                                }

data Map = Map [[Char]]

data Log = Log { __logRecords :: [String] }

data Stats = Stats [String]

data ItemMenu action = ItemMenu { __menuItems :: [(String, action)] }

makeLenses ''UIDesc

makeLenses ''Log

makeLenses ''ItemMenu

defaultMap = Map [[]]

defaultLog = Log []

defaultStats = Stats []

defaultItemMenu = ItemMenu []

defalutUIDesc = GameUIDesc defaultMap defaultLog defaultStats defaultItemMenu

mkGameUI :: State (UIDesc action) a -> UIDesc action
mkGameUI = flip execState defalutUIDesc

setMap :: [[Char]] -> State (UIDesc action) ()
setMap m = modify $ set _getMap (Map m)

setLog :: [String] -> State (UIDesc action) ()
setLog log = modify $ set _getLog (Log log)

setStats :: [String] -> State (UIDesc action) ()
setStats stats = modify $ set _getStats (Stats stats)

addToLog :: String -> State (UIDesc action) ()
addToLog item = modify $ over (_getLog . _logRecords) (item:)

addItem :: String -> action -> State (UIDesc action) ()
addItem item act = modify $ over (_itemMenu . _menuItems) ((item, act):)
