{-# LANGUAGE TemplateHaskell #-}

module UI.Descriptions.InventoryUIDesc where

import           Control.Lens
import           Control.Monad.State

data UIDesc action =
  Desc { __items :: [String]
       , __stats :: [(String, String)]
       , __onItemSelected :: Maybe (Int -> action)
       , __onClosed :: Maybe action
       , __selectedItem :: Maybe Int
       }

type Builder action = State (UIDesc action)

makeLenses ''UIDesc

defalutUIDesc :: UIDesc action
defalutUIDesc = Desc [] [] Nothing Nothing Nothing

mkInventoryUI :: Builder action a -> UIDesc action
mkInventoryUI = flip execState defalutUIDesc

addItem :: String -> Builder action ()
addItem item = modify $ over _items (item:)

addStat :: String -> String -> Builder action ()
addStat stat val = modify $ over _stats ((stat, val):)

setOnItemSelected :: (Int -> action) -> Builder action ()
setOnItemSelected f = modify $ set _onItemSelected (Just f)

setOnClosed :: action -> Builder action ()
setOnClosed f = modify $ set _onClosed (Just f)

selectItem :: Int -> Builder action ()
selectItem i = do
  len <- gets (length . __items)
  if i >= 0 && i < len
    then modify $ set _selectedItem (Just i)
    else modify $ set _selectedItem Nothing
