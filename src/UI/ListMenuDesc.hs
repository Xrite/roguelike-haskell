{-# LANGUAGE TemplateHaskell #-}
module UI.ListMenuDesc where

import           Control.Lens
import           Control.Monad.State

data UIDesc action = Desc { _title :: Title
                               , _items :: [ListItem]
                               , _onItemSelected :: Int -> action
                          }


data ListItem = ListItem String

data Title = Title String

makeLenses ''UIDesc

defaultTitle = Title ""

defalutUIDesc = Desc defaultTitle [] undefined 

mkInventoryUI :: State (UIDesc action) a -> UIDesc action
mkInventoryUI = flip execState defalutUIDesc

setTitle :: String -> State (UIDesc action) ()
setTitle str = modify $ set title (Title str)

addItem :: String -> State (UIDesc action) ()
addItem item = modify $ over items (ListItem item :)

