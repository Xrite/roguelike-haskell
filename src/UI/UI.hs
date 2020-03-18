{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module UI.UI where

import qualified UI.Descriptions.GameUIDesc as Game
import qualified UI.Descriptions.InventoryUIDesc as Inventory
import qualified UI.Descriptions.ListMenuDesc as ListMenu
import           Control.Lens
import           Control.Applicative

data UI a b =
  UIDesc { __baseLayout :: BaseLayout a b, __dialog :: Maybe (Dialog a b) }

data BaseLayout a b = GameUI (Game.UIDesc a b)
                  | InventoryUI (Inventory.UIDesc a b)
                  | MenuUI (ListMenu.UIDesc a b)
                  | EmptyLayout

data Dialog a b =
  Dialog { _dialogMessage :: String, _dialogOptions :: [(String, a -> b)] }

class HasUI a where
    currentUI :: a -> UI a a

makeLenses ''UI

makeLenses ''BaseLayout

makeLenses ''Dialog

baseLayout :: UI a b -> BaseLayout a b
baseLayout = __baseLayout

showDialog :: Dialog a b -> UI a b -> UI a b
showDialog d = undefined

makeDialog :: String -> [(String, a -> b)] -> Dialog a b
makeDialog = Dialog

makeGameUI :: Game.UIDesc a b -> UI a b
makeGameUI desc = UIDesc { __baseLayout = GameUI desc, __dialog = Nothing }

makeInventoryUI :: Inventory.UIDesc a b -> UI a b
makeInventoryUI
  desc = UIDesc { __baseLayout = InventoryUI desc, __dialog = Nothing }

makeMenuUI :: ListMenu.UIDesc a b -> UI a b
makeMenuUI desc = UIDesc { __baseLayout = MenuUI desc, __dialog = Nothing }