{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.UI where

import Control.Lens
import qualified UI.Descriptions.GameUIDesc as Game
import qualified UI.Descriptions.InventoryUIDesc as Inventory
import qualified UI.Descriptions.ListMenuDesc as ListMenu

data UI a
  = UIDesc (BaseLayout a)

data BaseLayout a
  = GameUI (Game.UIDesc a AnyHasUI)
  | InventoryUI (Inventory.UIDesc a AnyHasUI)
  | ListMenuUI (ListMenu.UIDesc a AnyHasUI)
  | End

class HasUI a where
  getUI :: a -> UI a

data AnyHasUI = forall a. HasUI a => AnyHasUI a

makeLenses ''UI

makeLenses ''BaseLayout

packHasUI :: HasUI a => a -> AnyHasUI
packHasUI = AnyHasUI

baseLayout :: UI a -> BaseLayout a
baseLayout (UIDesc l) = l

makeGameUI :: Game.Builder a AnyHasUI c -> UI a
makeGameUI builder = UIDesc $ GameUI $ Game.makeUI builder

makeInventoryUI :: Inventory.Builder a AnyHasUI c -> UI a
makeInventoryUI builder = UIDesc $ InventoryUI $ Inventory.makeUI builder

makeListMenuUI :: ListMenu.Builder a AnyHasUI c -> UI a
makeListMenuUI builder = UIDesc $ ListMenuUI $ ListMenu.makeUI builder

terminalUI :: UI a
terminalUI = UIDesc End
