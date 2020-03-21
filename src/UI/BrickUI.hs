module UI.BrickUI where

import           UI.UI as UI
import qualified UI.Descriptions.GameUIDesc as GameUI
import qualified UI.Descriptions.ListMenuDesc as ListMenu
import           Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border.Style as BS
import qualified UI.Keys as Keys
import qualified Graphics.Vty as V
import           Control.Lens
import           Data.Maybe

type Name = ()

data Tick = Tick

type UIState state = (state, (UI state))

app :: (HasUI a) => App (UIState a) Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

drawUI :: (HasUI a) => UIState a -> [Widget Name]
drawUI (state, ui) = case UI.baseLayout ui of
  UI.GameUI desc -> drawGameUI desc
  UI.InventoryUI desc -> undefined
  UI.MenuUI desc -> drawMenu desc
  UI.End -> [C.center $ str "game stopped"]

drawGameUI :: GameUI.UIDesc a b -> [Widget n]
drawGameUI desc =
  [ (drawMap (GameUI.getMap desc) <=> drawLog (GameUI.getLog desc))
      <+> (drawStats (GameUI.getStats desc)
           <=> drawItemMenu (GameUI.getItemMenu desc))]

drawMap :: GameUI.Map -> Widget n
drawMap m =
  withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Snake") $ vBox rows
  where
    rows = map str (GameUI.mapField m)

drawLog :: GameUI.Log -> Widget n
drawLog l = vBox rows
  where
    rows = [str s | (i, s) <- zip [0 ..] (GameUI.logRecords l)]

drawStats :: GameUI.Stats -> Widget n
drawStats _ = fill '#'

drawItemMenu :: GameUI.ItemMenu a b -> Widget n
drawItemMenu _ = fill '$'

drawMenu :: ListMenu.UIDesc a b -> [Widget n]
drawMenu menu = [vBox rows]
  where
    rows = [C.center
             $ str
             $ if ListMenu.selectedItem menu == Just i
               then s ++ "+"
               else s
           | (i, (ListMenu.ListItem s _)) <- zip [0 ..] (ListMenu.__items menu)]

handleEvent :: (HasUI a)
            => UIState a
            -> BrickEvent Name Tick
            -> EventM Name (Next (UIState a))
handleEvent (state, ui) event = case UI.baseLayout ui of
  UI.GameUI desc -> case event of
    (VtyEvent (V.EvKey V.KUp [])) -> continue $ pressArrow desc Keys.Up
    (VtyEvent (V.EvKey V.KDown [])) -> continue $ pressArrow desc Keys.Down
    (VtyEvent (V.EvKey V.KRight [])) -> continue $ pressArrow desc Keys.Right
    (VtyEvent (V.EvKey V.KLeft [])) -> continue $ pressArrow desc Keys.Left
    (VtyEvent (V.EvKey (V.KChar 'q') [])) -> continue $ pressKey desc (Keys.Letter 'q')
    _ -> continue (state, ui)
  UI.InventoryUI desc -> case event of
    (VtyEvent (V.EvKey V.KUp [])) -> continue $ undefined
    (VtyEvent (V.EvKey V.KDown [])) -> undefined
    _ -> continue (state, ui)
  UI.MenuUI desc -> case event of
    (VtyEvent (V.EvKey V.KUp []))
      -> continue (state, simpleListMenuUI $ ListMenu.moveSelectionUp desc)
    (VtyEvent (V.EvKey V.KDown []))
      -> continue (state, simpleListMenuUI $ ListMenu.moveSelectionDown desc)
    (VtyEvent (V.EvKey V.KEnter [])) -> continue . newUIState
      $ fromMaybe state (ListMenu.clickItem desc <*> pure state)
    _ -> continue (state, ui)
  UI.End -> halt (state, ui)
  where
    pressArrow desc arr = newUIState
      $ fromMaybe
        state
        (desc ^. GameUI._onArrowsPress <*> pure arr <*> pure state)
    pressKey desc key = newUIState
      $ fromMaybe
        state
        (desc ^. GameUI._onKeyPress <*> pure key <*> pure state)

    newUIState st = (st, currentUI st)

theMap :: AttrMap
theMap = attrMap V.defAttr []
