{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.UI where

import Control.Lens
import qualified UI.Descriptions.GameUIDesc as Game
import qualified UI.Descriptions.InventoryUIDesc as Inventory
import qualified UI.Descriptions.ListMenuDesc as ListMenu

data UI a where
  UIDesc :: BaseLayout a -> UI a

data BaseLayout a where
  GameUI :: (HasUI b) => Game.UIDesc a b -> BaseLayout a
  InventoryUI :: (HasUI b) => Inventory.UIDesc a b -> BaseLayout a
  ListMenuUI :: (HasUI b) => ListMenu.UIDesc a b -> BaseLayout a
  End :: BaseLayout a

data Dialog a
  = Dialog {_dialogMessage :: String, _dialogOptions :: [(String, a -> a)]}

class HasUI a where
  getUI :: a -> UI a

makeLenses ''UI

makeLenses ''BaseLayout

baseLayout :: UI a -> BaseLayout a
baseLayout (UIDesc l) = l

showDialog :: Dialog a -> UI a -> UI a
showDialog d = undefined

makeDialog :: String -> [(String, a -> a)] -> Dialog a
makeDialog = Dialog

makeGameUI :: (HasUI b) => Game.Builder a b c -> UI a
makeGameUI builder = UIDesc $ GameUI $ Game.makeUI builder

makeInventoryUI :: HasUI b => Inventory.Builder a b c -> UI a
makeInventoryUI builder = UIDesc $ InventoryUI $ Inventory.makeUI builder

makeListMenuUI :: HasUI b => ListMenu.Builder a b c -> UI a
makeListMenuUI builder = UIDesc $ ListMenuUI $ ListMenu.makeUI builder

terminalUI :: UI a
terminalUI = UIDesc End
