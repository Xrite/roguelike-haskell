{-# LANGUAGE TemplateHaskell #-}
module UI.InventoryUIDesc where

import Control.Lens
import Control.Monad.State

data UIDesc action = Desc { _items :: [String]
                         , _stats :: [(String, String)]
                         , _onItemSelected :: Int -> action
                         , _onClosed :: action
                         }

makeLenses ''UIDesc

defalutUIDesc = Desc [] [] undefined undefined 

mkInventoryUI :: State (UIDesc action) a -> UIDesc action
mkInventoryUI = flip execState defalutUIDesc

addItem :: String -> State (UIDesc action) ()
addItem item = modify $ over items (item :)

addStat :: String -> String -> State (UIDesc action) ()
addStat stat val = modify $ over stats ((stat, val) :)
