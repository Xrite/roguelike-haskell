{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module UI.Descriptions.GameUIDesc where

import           Control.Monad.State
import           Control.Lens
import qualified UI.Keys as Keys

data UIDesc a b =
  GameUIDesc { __getMap :: Map
             , __getLog :: Log
             , __getStats :: Stats
             , __itemMenu :: ItemMenu a b
             , __onArrowsPress :: Maybe (Keys.Arrows -> a -> b)
             , __onKeyPress :: Maybe (Keys.Keys -> a -> b)
             }

data Map = Map { __mapField :: [[Char]] }

data Log = Log { __logRecords :: [String] }

data Stats = Stats [String]

data ItemMenu a b = ItemMenu { __menuItems :: [(String, a -> b)] }

type Builder a b = State (UIDesc a b)

makeLenses ''UIDesc

makeLenses ''Log

makeLenses ''ItemMenu

defaultMap :: Map
defaultMap = Map [[]]

defaultLog :: Log
defaultLog = Log []

defaultStats :: Stats
defaultStats = Stats []

defaultItemMenu :: ItemMenu a b
defaultItemMenu = ItemMenu []

defalutUIDesc :: UIDesc a b
defalutUIDesc =
  GameUIDesc defaultMap defaultLog defaultStats defaultItemMenu Nothing Nothing

mkGameUI :: Builder a b c -> UIDesc a b
mkGameUI = flip execState defalutUIDesc

setMap :: [[Char]] -> Builder a b ()
setMap m = modify $ set _getMap (Map m)

setLog :: [String] -> Builder a b ()
setLog log = modify $ set _getLog (Log log)

setArrowPress :: (Keys.Arrows -> a -> b) -> Builder a b ()
setArrowPress f = modify $ set _onArrowsPress (Just f)

setKeyPress :: (Keys.Keys -> a -> b) -> Builder a b ()
setKeyPress f = modify $ set _onKeyPress (Just f)

setStats :: [String] -> Builder a b ()
setStats stats = modify $ set _getStats (Stats stats)

addToLog :: String -> Builder a b ()
addToLog item = modify $ over (_getLog . _logRecords) (item:)

addItem :: String -> (a -> b) -> Builder a b ()
addItem item f = modify $ over (_itemMenu . _menuItems) ((item, f):)

getMap :: UIDesc a b -> Map
getMap = __getMap

getLog :: UIDesc a b -> Log
getLog = __getLog

getStats :: UIDesc a b -> Stats
getStats = __getStats

getItemMenu :: UIDesc a b -> ItemMenu a b
getItemMenu = __itemMenu

getOnArrowsKeyPress :: UIDesc a b -> Maybe (Keys.Arrows -> a -> b)
getOnArrowsKeyPress = __onArrowsPress

getOnKeyPress :: UIDesc a b -> Maybe (Keys.Keys -> a -> b)
getOnKeyPress = __onKeyPress

mapField :: Map -> [[Char]]
mapField = __mapField

logRecords :: Log -> [String]
logRecords = __logRecords
