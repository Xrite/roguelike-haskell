{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.UI where

import Control.Lens
import qualified UI.Descriptions.GameUIDesc as Game
import qualified UI.Descriptions.InventoryUIDesc as Inventory
import qualified UI.Descriptions.ListMenuDesc as ListMenu

data UI m a
  = UIDesc (BaseLayout m a)

data BaseLayout m a
  = GameUI (Game.UIDesc a (m AnyHasIOUI))
  | InventoryUI (Inventory.UIDesc a (m AnyHasIOUI))
  | ListMenuUI (ListMenu.UIDesc a (m AnyHasIOUI))
  | End

class HasIOUI a where
  getUI :: a -> UI IO a

data AnyHasIOUI = forall m a. HasIOUI a => AnyHasIOUI a

makeLenses ''UI

makeLenses ''BaseLayout

packHasIOUI :: HasIOUI a => a -> AnyHasIOUI
packHasIOUI = AnyHasIOUI

baseLayout :: UI m a -> BaseLayout m a
baseLayout (UIDesc l) = l

makeGameUI :: (Applicative m) => Game.Builder a AnyHasIOUI c -> UI m a
makeGameUI builder = UIDesc . GameUI . fmap pure $ Game.makeUI builder

makeInventoryUI :: (Applicative m) => Inventory.Builder a AnyHasIOUI c -> UI m a
makeInventoryUI builder = UIDesc . InventoryUI . fmap pure $ Inventory.makeUI builder

makeListMenuUI :: (Applicative m) => ListMenu.Builder a AnyHasIOUI c -> UI m a
makeListMenuUI builder = UIDesc . ListMenuUI . fmap pure $ ListMenu.makeUI builder

terminalUI :: UI m a
terminalUI = UIDesc End
