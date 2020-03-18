{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module UI.Descriptions.GameUIDesc where

import           Control.Monad.State
import           Control.Lens
import qualified UI.Keys as Keys
import Game.Environment (Environment)

data GameUIDesc =
  GameUIDesc { __getMap :: MapDesc
             , __getLog :: Log
             }

newtype MapDesc = MapDesc { __mapField :: [String] }

newtype ListItem = ListItem String

newtype Title = Title String

data ListMenuUIDesc = ListMenuUIDesc { __title :: Title
                                     , __items :: [ListItem]
                                     }
newtype Log = Log { __logRecords :: [String] }

data UIDesc = GameUI GameUIDesc
            | MenuUI ListMenuUIDesc

data GameState = Game { __env :: Environment }
               | MainMenu


type Builder = State GameUIDesc

makeLenses ''UIDesc

makeLenses ''GameUIDesc

makeLenses ''Log

makeLenses ''MapDesc

defaultMap :: MapDesc
defaultMap = MapDesc [[]]

defaultLog :: Log
defaultLog = Log []

defaultItemMenu :: ListMenuUIDesc
defaultItemMenu = ListMenuUIDesc (Title "") []

defaultUIDesc :: GameUIDesc
defaultUIDesc = GameUIDesc defaultMap defaultLog

mkGameUI :: Builder c -> GameUIDesc
mkGameUI = flip execState defaultUIDesc

setMap :: [String] -> Builder ()
setMap m = modify $ set _getMap (MapDesc m)

setLog :: [String] -> Builder ()
setLog log = modify $ set _getLog (Log log)

addToLog :: String -> Builder ()
addToLog item = modify $ over (_getLog . _logRecords) (item:)

getMap :: GameUIDesc -> MapDesc
getMap = __getMap

getLog :: GameUIDesc -> Log
getLog = __getLog

mapField :: MapDesc -> [String]
mapField = __mapField

logRecords :: Log -> [String]
logRecords = __logRecords
