{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UI.Descriptions.GameUIDesc where

import Control.Lens
import Control.Monad.State
import Data.Array
import qualified UI.Keys as Keys
import Prelude hiding
  ( log,
    map,
  )

-- | A backend agnostic description of the game UI
data UIDesc e a b
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
        _onKeyPress :: Maybe (Keys.Keys -> a -> b),
        -- | Additional custom event handler
        _customEventHandler :: Maybe (e -> a -> b)
      }
  deriving (Functor)

-- | Map description
data Map
  = Map
      { -- | Terrain
        _mapTerrain :: Array (Int, Int) Char,
        -- | Is position has been seen by the player
        _mapHasBeenSeenByPlayer :: (Int, Int) -> Bool,
        -- | Is position visible to the player
        _mapIsVisibleToPlayer :: (Int, Int) -> Bool,
        -- | Mobs on the map with portrait
        _mapMobs :: [((Int, Int), Char)],
        -- | Players on the map with portrait
        _mapPlayers :: [((Int, Int), Char)],
        -- | Position of the player
        _mapPlayerPosition :: (Int, Int),
        -- | Portrait of the player
        _mapPlayerPortrait :: Char,
        -- | Entities on the map with icons
        _mapEntities :: [((Int, Int), Char)]
      }

-- | Log description
data Log
  = Log
      { -- | Log records
        _logRecords :: [String]
      }

-- | Stats description
data Stats
  = Stats
      { -- | Stats records
        _statsRecords :: [(String, String)]
      }

-- | Equipped items description
data EquippedItems
  = EquippedItems
      { -- | Equipped items the first field is the name of slot, the second field is the name of equipped item
        _equippedItemsSlots :: [(String, Maybe String)]
      }

-- | UIDesc builder
type Builder e a b = State (UIDesc e a b)

makeLenses ''UIDesc

makeLenses ''Map

makeLenses ''Log

makeLenses ''Stats

makeLenses ''EquippedItems

blankMap :: Map
blankMap =
  Map
    { _mapTerrain = undefined,
      _mapHasBeenSeenByPlayer = const False,
      _mapIsVisibleToPlayer = const False,
      _mapMobs = [],
      _mapPlayers = [],
      _mapPlayerPosition = undefined,
      _mapPlayerPortrait = undefined,
      _mapEntities = []
    }

blankLog :: Log
blankLog =
  Log
    { _logRecords = []
    }

blankStats :: Stats
blankStats =
  Stats
    { _statsRecords = []
    }

blankEquippedItems :: EquippedItems
blankEquippedItems =
  EquippedItems
    { _equippedItemsSlots = []
    }

blankUIDesc :: UIDesc e a b
blankUIDesc =
  GameUIDesc
    { _map = blankMap,
      _log = blankLog,
      _stats = blankStats,
      _equippedItems = blankEquippedItems,
      _onArrowPress = Nothing,
      _onKeyPress = Nothing,
      _customEventHandler = Nothing
    }

-- | Get an UIDesc from a builder
makeUI :: Builder e a b c -> UIDesc e a b
makeUI = flip execState blankUIDesc

-- | Set a map terrain in game UI
setMapTerrain :: Array (Int, Int) Char -> Builder e a b ()
setMapTerrain t = modify $ set (map . mapTerrain) t

-- | Set positions that have been seen by player
setMapHasBeenSeenByPlayer :: ((Int, Int) -> Bool) -> Builder e a b ()
setMapHasBeenSeenByPlayer seen = modify $ set (map . mapHasBeenSeenByPlayer) seen

-- | Set positions that are visible to the player
setMapIsVisibleToPlayer :: ((Int, Int) -> Bool) -> Builder e a b ()
setMapIsVisibleToPlayer visible = modify $ set (map . mapIsVisibleToPlayer) visible

-- | Set mobs on the map
setMapMobs :: [((Int, Int), Char)] -> Builder e a b ()
setMapMobs mobs = modify $ set (map . mapMobs) mobs

-- | Add a mob to the map
addMapMob :: (Int, Int) -> Char -> Builder e a b ()
addMapMob position portrait = modify $ over (map . mapMobs) ((position, portrait) :)

-- | Set players on the map
setMapPlayers :: [((Int, Int), Char)] -> Builder e a b ()
setMapPlayers players = modify $ set (map . mapPlayers) players

-- | Add a player to the map
addMapPlayer :: (Int, Int) -> Char -> Builder e a b ()
addMapPlayer position portrait = modify $ over (map . mapPlayers) ((position, portrait) :)

-- | Set the main player on the map
setMapMainPlayer :: (Int, Int) -> Char -> Builder e a b ()
setMapMainPlayer position portrait = do
  modify $ set (map . mapPlayerPosition) position
  modify $ set (map . mapPlayerPortrait) portrait

-- | Set entities on the map
setMapEntities :: [((Int, Int), Char)] -> Builder e a b ()
setMapEntities entities = modify $ set (map . mapEntities) entities

-- | Add a mob to the map
addMapEntity :: (Int, Int) -> Char -> Builder e a b ()
addMapEntity position icon = modify $ over (map . mapMobs) ((position, icon) :)

-- | Set log records
setLog :: [String] -> Builder e a b ()
setLog newLog = modify $ set log (Log newLog)

-- | Set an arrow press handler
setArrowPress :: (Keys.Arrows -> a -> b) -> Builder e a b ()
setArrowPress f = modify $ set onArrowPress (Just f)

-- | Set a key press handler
setKeyPress :: (Keys.Keys -> a -> b) -> Builder e a b ()
setKeyPress f = modify $ set onKeyPress (Just f)

setCustomEventHandler :: (e -> a -> b) -> Builder e a b ()
setCustomEventHandler f = modify $ set customEventHandler (Just f)

-- | Set stats records
setStats :: [(String, String)] -> Builder e a b ()
setStats newStats = modify $ set stats (Stats newStats)

-- | Add a log record to the end of the current log records
addToLog :: String -> Builder e a b ()
addToLog item = modify $ over (log . logRecords) (++ [item])

-- | Add an equipped item to the end of the current equipped items list
addEquippedItem :: String -> Maybe String -> Builder e a b ()
addEquippedItem slot item = modify $ over (equippedItems . equippedItemsSlots) (++ [(slot, item)])

-- | Set equipped items records
setEquippedItems :: [(String, Maybe String)] -> Builder e a b ()
setEquippedItems items = modify $ set equippedItems (EquippedItems items)