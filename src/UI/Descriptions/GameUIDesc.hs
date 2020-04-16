{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.Descriptions.GameUIDesc where

import           Control.Monad.State
import           Control.Lens
import           Prelude                 hiding ( map
                                                , log
                                                )
import qualified UI.Keys as Keys

data UIDesc a b =
  GameUIDesc { _map :: Map
             , _log :: Log
             , _stats :: Stats
             , _itemMenu :: ItemMenu a b
             , _onArrowPress :: Maybe (Keys.Arrows -> a -> b)
             , _onKeyPress :: Maybe (Keys.Keys -> a -> b)
             }

data Map = Map { _mapField :: [[Char]] }

data Log = Log { _logRecords :: [String] }

data Stats = Stats [String]

data ItemMenu a b = ItemMenu { _menuItems :: [(String, a -> b)] }

type Builder a b = State (UIDesc a b)

makeLenses ''UIDesc

makeLenses ''Map

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

makeUI :: Builder a b c -> UIDesc a b
makeUI = flip execState defalutUIDesc

setMap :: [[Char]] -> Builder a b ()
setMap m = modify $ set map (Map m)

setLog :: [String] -> Builder a b ()
setLog newLog = modify $ set log (Log newLog)

setArrowPress :: (Keys.Arrows -> a -> b) -> Builder a b ()
setArrowPress f = modify $ set onArrowPress (Just f)

setKeyPress :: (Keys.Keys -> a -> b) -> Builder a b ()
setKeyPress f = modify $ set onKeyPress (Just f)

setStats :: [String] -> Builder a b ()
setStats newStats = modify $ set stats (Stats newStats)

addToLog :: String -> Builder a b ()
addToLog item = modify $ over (log . logRecords) (item:)

addItem :: String -> (a -> b) -> Builder a b ()
addItem item f = modify $ over (itemMenu . menuItems) ((item, f):)

getMap :: UIDesc a b -> Map
getMap = _map

getLog :: UIDesc a b -> Log
getLog = _log

getStats :: UIDesc a b -> Stats
getStats = _stats

getItemMenu :: UIDesc a b -> ItemMenu a b
getItemMenu = _itemMenu

getOnArrowsKeyPress :: UIDesc a b -> Maybe (Keys.Arrows -> a -> b)
getOnArrowsKeyPress = _onArrowPress

getOnKeyPress :: UIDesc a b -> Maybe (Keys.Keys -> a -> b)
getOnKeyPress = _onKeyPress

getField :: Map -> [[Char]]
getField = _mapField
