{-# LANGUAGE TemplateHaskell #-}

module UI.Descriptions.GameUIDesc where

import           Control.Monad.State
import           Control.Lens
import qualified UI.Keys as Keys

data UIDesc action =
  GameUIDesc { __getMap :: Map
             , __getLog :: Log
             , __getStats :: Stats
             , __itemMenu :: ItemMenu action
             , __onArrowsPress :: Maybe (Keys.Arrows -> action)
             , __onKeyPress :: Maybe (Keys.Keys -> action)
             }

data Map = Map { __mapField :: [[Char]] }

data Log = Log { __logRecords :: [String] }

data Stats = Stats [String]

data ItemMenu action = ItemMenu { __menuItems :: [(String, action)] }

type Builder action = State (UIDesc action)

makeLenses ''UIDesc

makeLenses ''Log

makeLenses ''ItemMenu

defaultMap :: Map
defaultMap = Map [[]]

defaultLog :: Log
defaultLog = Log []

defaultStats :: Stats
defaultStats = Stats []

defaultItemMenu :: ItemMenu action
defaultItemMenu = ItemMenu []

defalutUIDesc :: UIDesc action
defalutUIDesc =
  GameUIDesc defaultMap defaultLog defaultStats defaultItemMenu Nothing Nothing

mkGameUI :: Builder action a -> UIDesc action
mkGameUI = flip execState defalutUIDesc

setMap :: [[Char]] -> Builder action ()
setMap m = modify $ set _getMap (Map m)

setLog :: [String] -> Builder action ()
setLog log = modify $ set _getLog (Log log)

setArrowPress :: (Keys.Arrows -> action) -> Builder action ()
setArrowPress f = modify $ set _onArrowsPress (Just f)

setKeyPress :: (Keys.Keys -> action) -> Builder action ()
setKeyPress f = modify $ set _onKeyPress (Just f)

setStats :: [String] -> Builder action ()
setStats stats = modify $ set _getStats (Stats stats)

addToLog :: String -> Builder action ()
addToLog item = modify $ over (_getLog . _logRecords) (item:)

addItem :: String -> action -> Builder action ()
addItem item act = modify $ over (_itemMenu . _menuItems) ((item, act):)

getMap :: UIDesc action -> Map
getMap = __getMap

getLog :: UIDesc action -> Log
getLog = __getLog

getStats :: UIDesc action -> Stats
getStats = __getStats

getItemMenu :: UIDesc action -> ItemMenu action
getItemMenu = __itemMenu

getOnArrowsKeyPress :: UIDesc action -> Maybe (Keys.Arrows -> action)
getOnArrowsKeyPress = __onArrowsPress

getOnKeyPress :: UIDesc action -> Maybe (Keys.Keys -> action)
getOnKeyPress = __onKeyPress

mapField :: Map -> [[Char]]
mapField = __mapField

logRecords :: Log -> [String]
logRecords = __logRecords
