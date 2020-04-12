{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module UI.Descriptions.ListMenuDesc where

import           Control.Lens
import           Control.Monad.State
import           Data.Maybe

data UIDesc a b = Desc { __title :: Title
                       , __items :: [ListItem a b]
                       , __selectedItem :: Maybe Int
                       }

data ListItem a b = ListItem { __name :: String, __function :: a -> b }

data Title = Title String

type Builder a b = State (UIDesc a b)

makeLenses ''UIDesc

defaultTitle :: Title
defaultTitle = Title ""

defaultUIDesc :: UIDesc a b
defaultUIDesc = Desc defaultTitle [] Nothing

makeUI :: Builder a b c -> UIDesc a b
makeUI = flip execState defaultUIDesc

setTitle :: String -> Builder a b ()
setTitle str = modify $ set _title (Title str)

addItem :: String -> (a -> b) -> Builder a b ()
addItem name f = modify $ over _items (++ [ListItem name f])

selectItem :: Int -> Builder a b ()
selectItem i = do
  len <- gets (length . __items)
  if i >= 0 && i < len
    then modify $ set _selectedItem (Just i)
    else modify $ set _selectedItem Nothing

moveSelectionUp :: UIDesc a b -> UIDesc a b
moveSelectionUp = over
  _selectedItem
  (fmap
   $ \i -> if i > 0
           then i - 1
           else i)

moveSelectionDown :: UIDesc a b -> UIDesc a b
moveSelectionDown desc = over
  _selectedItem
  (fmap
   $ \i -> if i < length (__items desc) - 1
           then i + 1
           else i)
  desc

clickItem :: UIDesc a b -> Maybe (a -> b)
clickItem desc = __function
  <$> ((!!) <$> pure (__items desc) <*> __selectedItem desc)

selectedItem :: UIDesc a b -> Maybe Int
selectedItem = __selectedItem
