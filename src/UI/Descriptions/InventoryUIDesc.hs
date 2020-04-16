{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module UI.Descriptions.InventoryUIDesc where

import           Control.Lens
import           Control.Monad.State

data UIDesc a b = Desc { _items :: [String]
                       , _stats :: [(String, String)]
                       , _onItemSelected :: Maybe (Int -> a -> b)
                       , _onClosed :: Maybe (a -> b)
                       , _selectedItem :: Maybe Int
                       }

type Builder a b = State (UIDesc a b)

makeLenses ''UIDesc

defalutUIDesc :: UIDesc a b
defalutUIDesc = Desc [] [] Nothing Nothing Nothing

makeUI :: Builder a b c -> UIDesc a b
makeUI = flip execState defalutUIDesc

addItem :: String -> Builder a b ()
addItem item = modify $ over items (item :)

addStat :: String -> String -> Builder a b ()
addStat stat val = modify $ over stats ((stat, val) :)

setOnItemSelected :: (Int -> a -> b) -> Builder a b ()
setOnItemSelected f = modify $ set onItemSelected (Just f)

setOnClosed :: (a -> b) -> Builder a b ()
setOnClosed f = modify $ set onClosed (Just f)

selectItem :: Int -> Builder a b ()
selectItem i = do
  len <- gets (length . _items)
  if i >= 0 && i < len
    then modify $ set selectedItem (Just i)
    else modify $ set selectedItem Nothing
