module UI.BrickUI where

import qualified UI.UI as UI
import qualified UI.Descriptions.GameUIDesc as GameUI
import           Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
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
  UI.GameUI desc      -> drawGameUI desc
  UI.InventoryUI desc -> undefined
  UI.MenuUI desc      -> undefined
  UI.EmptyLayout      -> undefined

drawGameUI :: GameUI.UIDesc action -> [Widget n]
drawGameUI desc =
  [ (drawMap (GameUI.getMap desc) <=> drawLog (GameUI.getLog desc))
      <+> (drawStats (GameUI.getStats desc)
           <=> drawItemMenu (GameUI.getItemMenu desc))]


drawMap :: GameUI.Map -> Widget n
drawMap m = withBorderStyle BS.unicodeBold $
  B.borderWithLabel (str "Snake") $
  vBox rows
  where
    rows = map str (GameUI.mapField m)

drawLog :: GameUI.Log -> Widget n
drawLog l = vBox rows
  where
    rows = map str $ GameUI.logRecords l

drawStats :: GameUI.Stats -> Widget n
drawStats _ = fill '#'

drawItemMenu :: GameUI.ItemMenu action -> Widget n
drawItemMenu _ = fill '$' 

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
