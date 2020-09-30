{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.Descriptions.ListMenuDesc where

import Control.Lens
import Control.Monad.State

data UIDesc e a b
  = Desc
      { _title :: Title,
        _items :: [ListItem a b],
        _selectedItem :: Maybe Int,
        -- | Additional custom event handler
        _customEventHandler :: Maybe (e -> a -> b)
      }
  deriving (Functor)

data ListItem a b = ListItem {_listItemName :: String, _listItemFunction :: (a -> b)}
  deriving (Functor)

data Title = Title {_titleText :: String}

type Builder e a b = State (UIDesc e a b)

makeLenses ''UIDesc

defaultTitle :: Title
defaultTitle = Title ""

defaultUIDesc :: UIDesc e a b
defaultUIDesc =
  Desc
    { _title = defaultTitle,
      _items = [],
      _selectedItem = Nothing,
      _customEventHandler = Nothing
    }

makeUI :: Builder e a b c -> UIDesc e a b
makeUI = flip execState defaultUIDesc

setTitle :: String -> Builder e a b ()
setTitle str = modify $ set title (Title str)

addItem :: String -> (a -> b) -> Builder e a b ()
addItem name f = modify $ over items (++ [ListItem name f])

addItemPure :: Applicative m => String -> (a -> b) -> Builder e a (m b) ()
addItemPure name f = modify $ over items (++ [ListItem name $ pure . f])

addCustomEventHandler :: (e -> a -> b) -> Builder e a b ()
addCustomEventHandler f = modify $ set customEventHandler (Just f)

selectItem :: Int -> Builder e a b ()
selectItem i = do
  len <- gets (length . _items)
  if i >= 0 && i < len
    then modify $ set selectedItem (Just i)
    else modify $ set selectedItem Nothing

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
          if i < length (_items desc) - 1
            then i + 1
            else i
    )
    desc

clickItem :: UIDesc e a b -> Maybe (a -> b)
clickItem desc =
  _listItemFunction
    <$> ((!!) <$> pure (desc ^. items) <*> desc ^. selectedItem)

getTitle :: UIDesc e a b -> Title
getTitle = _title

getItems :: UIDesc e a b -> [ListItem a b]
getItems = _items

getSelectedItem :: UIDesc e a b -> Maybe Int
getSelectedItem = _selectedItem
