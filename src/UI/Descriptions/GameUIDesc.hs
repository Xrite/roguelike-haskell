{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.Descriptions.GameUIDesc where

import Control.Lens
import Control.Monad.State
import qualified UI.Keys as Keys
import Prelude hiding
  ( log,
    map,
  )

-- | A backend agnostic description of the game UI
data UIDesc a b
  = GameUIDesc
      { -- | Game map
        _map :: Map,
        -- | Log
        _log :: Log,
        -- | Stats
        _stats :: Stats,
        -- | Equipped items
        _equippedItems :: EquippedItems,
        -- | Arrow press handler
        _onArrowPress :: Maybe (Keys.Arrows -> a -> b),
        -- | Key press handler
        _onKeyPress :: Maybe (Keys.Keys -> a -> b)
      }
  deriving (Functor)

data Map = Map {_mapField :: [[Char]]}

data Log = Log {_logRecords :: [String]}

data Stats = Stats {_statsRecords :: [(String, String)]}

data EquippedItems = EquippedItems {_equippedItemsSlots :: [(String, Maybe String)]}

type Builder a b = State (UIDesc a b)

makeLenses ''UIDesc

makeLenses ''Map

makeLenses ''Log

makeLenses ''Stats

makeLenses ''EquippedItems

blankMap :: Map
blankMap = Map [[]]

blankLog :: Log
blankLog = Log []

blankStats :: Stats
blankStats = Stats []

blankEquippedItems :: EquippedItems
blankEquippedItems = EquippedItems []

blankUIDesc :: UIDesc a b
blankUIDesc =
  GameUIDesc blankMap blankLog blankStats blankEquippedItems Nothing Nothing

-- | Get an UIDesc from a builder
makeUI :: Builder a b c -> UIDesc a b
makeUI = flip execState blankUIDesc

-- | Set a map in game UI
setMap :: [[Char]] -> Builder a b ()
setMap m = modify $ set map (Map m)

-- | Set log records
setLog :: [String] -> Builder a b ()
setLog newLog = modify $ set log (Log newLog)

-- | Set an arrow press handler
setArrowPress :: (Keys.Arrows -> a -> b) -> Builder a b ()
setArrowPress f = modify $ set onArrowPress (Just f)

-- | Set a key press handler
setKeyPress :: (Keys.Keys -> a -> b) -> Builder a b ()
setKeyPress f = modify $ set onKeyPress (Just f)

-- | Set stats records
setStats :: [(String, String)] -> Builder a b ()
setStats newStats = modify $ set stats (Stats newStats)

-- | Add a log record to the end of the current log records
addToLog :: String -> Builder a b ()
addToLog item = modify $ over (log . logRecords) (++ [item])

-- | Add an equipped item to the end of the current equipped items list
addEquippedItem :: String -> Maybe String -> Builder a b ()
addEquippedItem slot item = modify $ over (equippedItems . equippedItemsSlots) (++ [(slot, item)])

-- | Set equipped items records
setEquippedItems :: [(String, Maybe String)] -> Builder a b ()
setEquippedItems items = modify $ set equippedItems (EquippedItems items)
