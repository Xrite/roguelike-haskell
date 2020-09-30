{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.UI where

import Control.Lens
import qualified UI.Descriptions.GameUIDesc as Game
import qualified UI.Descriptions.InventoryUIDesc as Inventory
import qualified UI.Descriptions.ListMenuDesc as ListMenu
import qualified UI.Descriptions.EnterDataUIDesc as EnterData

-- |
newtype UI m a e = UIDesc (BaseLayout m a e)

data BaseLayout m a e
  = GameUI (Game.UIDesc e a (m (AnyHasUI m e)))
  | InventoryUI (Inventory.UIDesc e a (m (AnyHasUI m e)))
  | ListMenuUI (ListMenu.UIDesc e a (m (AnyHasUI m e)))
  | EnterDataUI (EnterData.UIDesc e a (m (AnyHasUI m e)))
  | End

class HasUI m a e where
  getUI :: a -> UI m a e

data AnyHasUI m e where 
  AnyHasUI :: HasUI m a e => a -> AnyHasUI m e

makeLenses ''UI

makeLenses ''BaseLayout

packHasIOUI :: HasUI m a e => a -> AnyHasUI m e
packHasIOUI = AnyHasUI

baseLayout :: UI m a e -> BaseLayout m a e
baseLayout (UIDesc l) = l

makeGameUIPure :: (Applicative m) => Game.Builder e a (AnyHasUI m e) c -> UI m a e
makeGameUIPure builder = UIDesc . GameUI . fmap pure $ Game.makeUI builder

makeGameUI :: Game.Builder e a (m (AnyHasUI m e)) c -> UI m a e
makeGameUI builder = UIDesc . GameUI $ Game.makeUI builder

makeInventoryUIPure :: (Applicative m) => Inventory.Builder e a (AnyHasUI m e) c -> UI m a e
makeInventoryUIPure builder = UIDesc . InventoryUI . fmap pure $ Inventory.makeUI builder

makeInventoryUI :: Inventory.Builder e a (m (AnyHasUI m e)) c -> UI m a e
makeInventoryUI builder = UIDesc . InventoryUI $ Inventory.makeUI builder

makeListMenuUIPure :: (Applicative m) => ListMenu.Builder e a (AnyHasUI m e) c -> UI m a e
makeListMenuUIPure builder = UIDesc . ListMenuUI . fmap pure $ ListMenu.makeUI builder

makeListMenuUI :: ListMenu.Builder e a (m (AnyHasUI m e)) c -> UI m a e
makeListMenuUI builder = UIDesc . ListMenuUI $ ListMenu.makeUI builder

makeEnterDataUI :: EnterData.Builder e a (m (AnyHasUI m e)) c -> UI m a e
makeEnterDataUI builder = UIDesc . EnterDataUI $ EnterData.makeUI builder

terminalUI :: UI m a e
terminalUI = UIDesc End
