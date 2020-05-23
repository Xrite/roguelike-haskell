{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.Descriptions.InventoryUIDesc where

import Control.Lens
import Control.Monad.State

data Frame = First | Second deriving (Eq)

data UIDesc e a b
  = Desc
      { _items :: [String],
        _slots :: [(String, String)],
        _stats :: [(String, String)],
        _onItemSelected :: Maybe (Int -> a -> b),
        _onSlotSelected :: Maybe (Int -> a -> b),
        _onClosed :: Maybe (a -> b),
        _selectedItem :: Maybe Int,
        _selectedFrame :: Frame,
        -- | Additional custom event handler
        _customEventHandler :: Maybe (e -> a -> b)
      }
  deriving (Functor)

type Builder e a b = State (UIDesc e a b)

makeLenses ''UIDesc

defalutUIDesc :: UIDesc e a b
defalutUIDesc =
  Desc
    { _items = [],
      _slots = [],
      _stats = [],
      _onItemSelected = Nothing,
      _onSlotSelected = Nothing,
      _onClosed = Nothing,
      _selectedItem = Nothing,
      _selectedFrame = Second,
      _customEventHandler = Nothing
    }

makeUI :: Builder e a b c -> UIDesc e a b
makeUI = flip execState defalutUIDesc

addItem :: String -> Builder e a b ()
addItem item = modify $ over items (item :)

addStat :: String -> String -> Builder e a b ()
addStat stat val = modify $ over stats ((stat, val) :)

setSlots :: [(String, String)] -> Builder e a b ()
setSlots ss = modify $ set slots ss

setItems :: [String] -> Builder e a b ()
setItems is = modify $ set items is

setStats :: [(String, String)] -> Builder e a b ()
setStats ss = modify $ set stats ss

setOnSlotSelected :: (Int -> a -> b) -> Builder e a b ()
setOnSlotSelected f = modify $ set onSlotSelected (Just f)

setOnItemSelected :: (Int -> a -> b) -> Builder e a b ()
setOnItemSelected f = modify $ set onItemSelected (Just f)

setOnClosed :: (a -> b) -> Builder e a b ()
setOnClosed f = modify $ set onClosed (Just f)

addCustomEventHandler :: (e -> a -> b) -> Builder e a b ()
addCustomEventHandler f = modify $ set customEventHandler (Just f)

-- | Select an item with given index. If an index is invalid then no item is selected
selectItem :: Int -> Builder e a b ()
selectItem i = do
  len <- gets (length . _items)
  if i >= 0 && i < len
    then modify $ set selectedItem (Just i)
    else modify $ set selectedItem Nothing

-- | Select a slot with given index. If the index is invalid then no slot is selected
selectFrame :: Frame -> Builder e a b ()
selectFrame f = modify $ set selectedFrame f

switchSelection :: UIDesc e a b -> UIDesc e a b
switchSelection desc = case desc ^. selectedFrame of
  First -> set selectedItem (Just 0) $ set selectedFrame Second desc
  Second -> set selectedItem (Just 0) $ set selectedFrame First desc

moveSelectionUp :: UIDesc e a b -> UIDesc e a b
moveSelectionUp =
  over
    selectedItem
    ( fmap $
        \i ->
          if i > 0
            then i - 1
            else i
    )

moveSelectionDown :: UIDesc e a b -> UIDesc e a b
moveSelectionDown desc =
  over
    selectedItem
    ( fmap $
        \i ->
          if i < threshold
            then i + 1
            else i
    )
    desc
  where
    threshold = case desc ^. selectedFrame of
      First -> length $ desc ^. items
      Second -> length $ desc ^. slots

onSelected :: UIDesc e a b -> Maybe (Int -> a -> b)
onSelected desc = case desc ^. selectedFrame of
  First -> desc ^. onItemSelected
  Second -> desc ^. onSlotSelected
