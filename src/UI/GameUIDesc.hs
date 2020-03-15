{-# LANGUAGE TemplateHaskell #-}
module UI.GameUIDesc
    ( UIDesc
    )
where

import           Control.Monad.State
import           Control.Lens

data UIDesc action = GameUIDesc { _getMap :: Map
                         , _getLog :: Log
                         , _getStats :: Stats
                         , _itemMenu :: ItemMenu action
                         }


data Map = Map [[Char]]

data Log = Log {_logRecords :: [String]}

data Stats = Stats [String]

data ItemMenu action = ItemMenu {_menuItems :: [(String, action)]}

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
setMap m = modify $ set getMap (Map m)

setLog :: [String] -> State (UIDesc action) ()
setLog log = modify $ set getLog (Log log)

setStats :: [String] -> State (UIDesc action) ()
setStats stats = modify $ set getStats (Stats stats)

addToLog :: String -> State (UIDesc action) ()
addToLog item = modify $ over (getLog . logRecords) (item :)

addItem :: String -> action -> State (UIDesc action) ()
addItem item act = modify $ over (itemMenu . menuItems) ((item, act) :)