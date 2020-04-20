{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.UI where

import Control.Lens
import qualified UI.Descriptions.GameUIDesc as Game
import qualified UI.Descriptions.InventoryUIDesc as Inventory
import qualified UI.Descriptions.ListMenuDesc as ListMenu

newtype UI m a = UIDesc (BaseLayout m a)

data BaseLayout m a
  = GameUI (Game.UIDesc a (m (AnyHasUI m)))
  | InventoryUI (Inventory.UIDesc a (m (AnyHasUI m)))
  | ListMenuUI (ListMenu.UIDesc a (m (AnyHasUI m)))
  | End

class HasUI m a where
  getUI :: a -> UI m a

data AnyHasUI m = forall a. HasUI m a => AnyHasUI a

makeLenses ''UI

makeLenses ''BaseLayout

packHasIOUI :: HasUI m a => a -> AnyHasUI m
packHasIOUI = AnyHasUI

baseLayout :: UI m a -> BaseLayout m a
baseLayout (UIDesc l) = l

makeGameUIPure :: (Applicative m) => Game.Builder a (AnyHasUI m) c -> UI m a
makeGameUIPure builder = UIDesc . GameUI . fmap pure $ Game.makeUI builder

makeGameUI :: Game.Builder a (m (AnyHasUI m)) c -> UI m a
makeGameUI builder = UIDesc . GameUI $ Game.makeUI builder

makeInventoryUIPure :: (Applicative m) => Inventory.Builder a (AnyHasUI m) c -> UI m a
makeInventoryUIPure builder = UIDesc . InventoryUI . fmap pure $ Inventory.makeUI builder

makeInventoryUI :: Inventory.Builder a (m (AnyHasUI m)) c -> UI m a
makeInventoryUI builder = UIDesc . InventoryUI $ Inventory.makeUI builder

makeListMenuUIPure :: (Applicative m) => ListMenu.Builder a (AnyHasUI m) c -> UI m a
makeListMenuUIPure builder = UIDesc . ListMenuUI . fmap pure $ ListMenu.makeUI builder

makeListMenuUI :: ListMenu.Builder a (m (AnyHasUI m)) c -> UI m a
makeListMenuUI builder = UIDesc . ListMenuUI $ ListMenu.makeUI builder

terminalUI :: UI m a
terminalUI = UIDesc End
