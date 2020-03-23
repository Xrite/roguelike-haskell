{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}

module UI.UI where

import qualified UI.Descriptions.GameUIDesc as Game
import qualified UI.Descriptions.InventoryUIDesc as Inventory
import qualified UI.Descriptions.ListMenuDesc as ListMenu
import           Control.Lens

data UI a =
  UIDesc { __baseLayout :: BaseLayout a, __dialog :: Maybe (Dialog a) }

data BaseLayout a = GameUI (Game.UIDesc a a)
                  | InventoryUI (Inventory.UIDesc a a)
                  | MenuUI (ListMenu.UIDesc a a)
                  | End

data Dialog a =
  Dialog { _dialogMessage :: String, _dialogOptions :: [(String, a -> a)] }

class HasUI a where
  currentUI :: a -> UI a

makeLenses ''UI

makeLenses ''BaseLayout

baseLayout :: UI a -> BaseLayout a
baseLayout = __baseLayout

showDialog :: Dialog a -> UI a -> UI a
showDialog d = undefined

makeDialog :: String -> [(String, a -> a)] -> Dialog a
makeDialog = Dialog

makeGameUI :: Game.Builder a a () -> UI a
makeGameUI builder =
  UIDesc { __baseLayout = GameUI $ Game.makeUI builder, __dialog = Nothing }

makeInventoryUI :: Inventory.Builder a a () -> UI a
makeInventoryUI builder =
  UIDesc { __baseLayout = InventoryUI $ Inventory.makeUI builder
         , __dialog = Nothing
         }

makeMenuUI :: ListMenu.Builder a a () -> UI a
makeMenuUI builder = UIDesc { __baseLayout = MenuUI $ ListMenu.makeUI builder
                            , __dialog = Nothing
                            }

simpleListMenuUI :: ListMenu.UIDesc a a -> UI a
simpleListMenuUI desc = UIDesc (MenuUI desc) Nothing 

terminalUI :: UI a
terminalUI = UIDesc End Nothing 