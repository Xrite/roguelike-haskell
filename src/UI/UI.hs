{-# LANGUAGE TemplateHaskell #-}

module UI.UI where

import qualified UI.Descriptions.GameUIDesc as Game
import qualified UI.Descriptions.InventoryUIDesc as Inventory
import qualified UI.Descriptions.ListMenuDesc as ListMenu
import           Control.Lens

data UI action =
  UI { __baseLayout :: BaseLayout action, __dialog :: Maybe (Dialog action) }

data BaseLayout action = GameUI (Game.UIDesc action)
                       | InventoryUI (Inventory.UIDesc action)
                       | MenuUI (ListMenu.UIDesc action)
                       | EmptyLayout

data Dialog action =
  Dialog { _dialogMessage :: String, _dialogOptions :: [(String, action)] }

makeLenses ''UI

makeLenses ''BaseLayout

makeLenses ''Dialog

showDialog :: Dialog action -> UI action -> UI action
showDialog d = set _dialog $ Just d

makeDialog :: String -> [(String, action)] -> Dialog action
makeDialog = Dialog

makeGameUI :: Game.UIDesc action -> UI action
makeGameUI desc = UI { __baseLayout = GameUI desc, __dialog = Nothing }

makeInventoryUI :: Inventory.UIDesc action -> UI action
makeInventoryUI desc = UI { __baseLayout = InventoryUI desc, __dialog = Nothing }

makeMenuUI :: ListMenu.UIDesc action -> UI action
makeMenuUI desc = UI { __baseLayout = MenuUI desc, __dialog = Nothing }
