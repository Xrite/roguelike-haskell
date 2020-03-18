{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module UI.Descriptions.ListMenuDesc where

import           Control.Lens
import           Control.Monad.State

data UIDesc a b = Desc { __title :: Title
                          , __items :: [ListItem]
                          , __onItemSelected :: Maybe (Int -> a -> b)
                          , __selectedItem :: Maybe Int
                          }

data ListItem = ListItem String

data Title = Title String

type Builder a b = State (UIDesc a b)

makeLenses ''UIDesc

defaultTitle = Title ""

defalutUIDesc = Desc defaultTitle [] Nothing Nothing

mkInventoryUI :: Builder a b c -> UIDesc a b
mkInventoryUI = flip execState defalutUIDesc

setTitle :: String -> Builder a b ()
setTitle str = modify $ set _title (Title str)

addItem :: String -> Builder a b ()
addItem item = modify $ over _items (ListItem item:)

selectItem :: Int -> Builder a b ()
selectItem i = do
  len <- gets (length . __items)
  if i >= 0 && i < len
    then modify $ set _selectedItem (Just i)
    else modify $ set _selectedItem Nothing
