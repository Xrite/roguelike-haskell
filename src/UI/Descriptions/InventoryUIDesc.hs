{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module UI.Descriptions.InventoryUIDesc where

import           Control.Lens
import           Control.Monad.State

data UIDesc a b = Desc { __items :: [String]
                          , __stats :: [(String, String)]
                          , __onItemSelected :: Maybe (Int -> a -> b)
                          , __onClosed :: Maybe (a -> b)
                          , __selectedItem :: Maybe Int
                          }

type Builder a b = State (UIDesc a b)

makeLenses ''UIDesc

defalutUIDesc :: UIDesc a b
defalutUIDesc = Desc [] [] Nothing Nothing Nothing

mkInventoryUI :: Builder a b c -> UIDesc a b
mkInventoryUI = flip execState defalutUIDesc

addItem :: String -> Builder a b ()
addItem item = modify $ over _items (item:)

addStat :: String -> String -> Builder a b ()
addStat stat val = modify $ over _stats ((stat, val):)

setOnItemSelected :: (Int -> a -> b) -> Builder a b ()
setOnItemSelected f = modify $ set _onItemSelected (Just f)

setOnClosed :: (a -> b) -> Builder a b ()
setOnClosed f = modify $ set _onClosed (Just f)

selectItem :: Int -> Builder a b ()
selectItem i = do
  len <- gets (length . __items)
  if i >= 0 && i < len
    then modify $ set _selectedItem (Just i)
    else modify $ set _selectedItem Nothing
