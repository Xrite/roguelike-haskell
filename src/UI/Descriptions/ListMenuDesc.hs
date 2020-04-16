{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module UI.Descriptions.ListMenuDesc where

import           Control.Lens
import           Control.Monad.State

data UIDesc a b = Desc { _title :: Title
                       , _items :: [ListItem a b]
                       , _selectedItem :: Maybe Int
                       }

data ListItem a b = ListItem { _listItemName :: String, _listItemFunction :: (a -> b) }

data Title = Title {_titleText :: String}

type Builder a b = State (UIDesc a b)

makeLenses ''UIDesc

defaultTitle :: Title
defaultTitle = Title ""

defaultUIDesc :: UIDesc a b
defaultUIDesc = Desc defaultTitle [] Nothing

makeUI :: Builder a b c -> UIDesc a b
makeUI = flip execState defaultUIDesc

setTitle :: String -> Builder a b ()
setTitle str = modify $ set title (Title str)

addItem :: String -> (a -> b) -> Builder a b ()
addItem name f = modify $ over items (++ [ListItem name f])

selectItem :: Int -> Builder a b ()
selectItem i = do
  len <- gets (length . _items)
  if i >= 0 && i < len
    then modify $ set selectedItem (Just i)
    else modify $ set selectedItem Nothing

moveSelectionUp :: UIDesc a b -> UIDesc a b
moveSelectionUp = over
  selectedItem
  (fmap
   $ \i -> if i > 0
           then i - 1
           else i)

moveSelectionDown :: UIDesc a b -> UIDesc a b
moveSelectionDown desc = over
  selectedItem
  (fmap
   $ \i -> if i < length (_items desc) - 1
           then i + 1
           else i)
  desc

clickItem :: UIDesc a b -> Maybe (a -> b)
clickItem desc = _listItemFunction
  <$> ((!!) <$> pure (desc ^. items) <*> desc ^. selectedItem)

getTitle :: UIDesc a b -> Title
getTitle = _title

getItems :: UIDesc a b -> [ListItem a b]
getItems = _items

getSelectedItem :: UIDesc a b -> Maybe Int
getSelectedItem = _selectedItem
