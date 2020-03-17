{-# LANGUAGE TemplateHaskell #-}

module UI.Descriptions.ListMenuDesc where

import           Control.Lens
import           Control.Monad.State

data UIDesc action = Desc { __title :: Title
                          , __items :: [ListItem]
                          , __onItemSelected :: Maybe (Int -> action)
                          , __selectedItem :: Maybe Int
                          }

data ListItem = ListItem String

data Title = Title String

type Builder action = State (UIDesc action)

makeLenses ''UIDesc

defaultTitle = Title ""

defalutUIDesc = Desc defaultTitle [] Nothing Nothing 

mkInventoryUI :: Builder action a -> UIDesc action
mkInventoryUI = flip execState defalutUIDesc

setTitle :: String -> Builder action ()
setTitle str = modify $ set _title (Title str)

addItem :: String -> Builder action ()
addItem item = modify $ over _items (ListItem item:)

selectItem :: Int -> Builder action ()
selectItem i = do
  len <- gets (length . __items)
  if i >= 0 && i < len
    then modify $ set _selectedItem (Just i)
    else modify $ set _selectedItem Nothing