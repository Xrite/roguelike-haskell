module UI
    ( UI(..)
    )
where

import qualified UI.GameUIDesc                 as G
import qualified UI.InventoryUIDesc            as I
import qualified UI.ListMenuDesc               as LM

data UI ctx = GameUI G.UIDesc
            | InventoryUI (I.UIDesc ctx)
            | MainMenuUI (LM.UIDesc ctx)
