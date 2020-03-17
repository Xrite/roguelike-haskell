module UI.BrickUI where

import qualified UI.UI as UI
import           Brick
import           Game 
import qualified Graphics.Vty as V

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
drawUI game = case UI._baseLayout . currentUI $ game of
  UI.GameUI desc      -> undefined
  UI.InventoryUI desc -> undefined 
  UI.MenuUI desc      -> undefined 
  UI.EmptyLayout      -> undefined 

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent game event = case UI._baseLayout . currentUI $ game of
  UI.GameUI desc      -> undefined
  UI.InventoryUI desc -> case event of
    (VtyEvent (V.EvKey V.KUp []))   -> undefined
    (VtyEvent (V.EvKey V.KDown [])) -> undefined
  UI.MenuUI desc      -> case event of
    (VtyEvent (V.EvKey V.KUp []))   -> undefined
    (VtyEvent (V.EvKey V.KDown [])) -> undefined
  UI.EmptyLayout      -> continue game

theMap :: AttrMap
theMap = undefined
