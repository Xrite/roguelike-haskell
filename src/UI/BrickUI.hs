module UI.BrickUI where

import qualified UI.UI as UI
import qualified UI.Descriptions.GameUIDesc as GameUI
import           Brick
import           Game
import qualified UI.Keys as Keys
import qualified Graphics.Vty as V
import           Control.Lens
import           Data.Maybe

type Name = ()

data Tick = Tick

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

drawUI :: Game -> [Widget Name]
drawUI game = case UI.baseLayout . currentUI $ game of
  UI.GameUI desc      -> undefined
  UI.InventoryUI desc -> undefined
  UI.MenuUI desc      -> undefined
  UI.EmptyLayout      -> undefined

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent game event = case ui of
  UI.GameUI desc      -> case event of
    (VtyEvent (V.EvKey V.KUp [])) -> continue $ pressArrow desc Keys.Up
    (VtyEvent (V.EvKey V.KDown [])) -> continue $ pressArrow desc Keys.Down
    (VtyEvent (V.EvKey V.KRight [])) -> continue $ pressArrow desc Keys.Right
    (VtyEvent (V.EvKey V.KLeft [])) -> continue $ pressArrow desc Keys.Left
    _ -> continue game
  UI.InventoryUI desc -> case event of
    (VtyEvent (V.EvKey V.KUp [])) -> undefined
    (VtyEvent (V.EvKey V.KDown [])) -> undefined
    _ -> continue game
  UI.MenuUI desc      -> case event of
    (VtyEvent (V.EvKey V.KUp [])) -> undefined
    (VtyEvent (V.EvKey V.KDown [])) -> undefined
    _ -> continue game
  UI.EmptyLayout      -> continue game
  where
    ui = UI.baseLayout . currentUI $ game

    pressArrow desc arr =
      fromMaybe game (desc ^. GameUI._onArrowsPress <*> pure arr <*> pure game)


theMap :: AttrMap
theMap = undefined
