{-# LANGUAGE ExistentialQuantification #-}

module UI.BrickUI where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens
import Data.Maybe
import qualified Graphics.Vty as V
import qualified UI.Descriptions.GameUIDesc as GameUI
import qualified UI.Descriptions.ListMenuDesc as ListMenu
import qualified UI.Keys as Keys
import UI.UI as UI

type Name = ()

data Tick = Tick

data UIState = forall s. HasUI s => UIState s (UI s)

packUIState :: HasUI s => s -> UI s -> UIState
packUIState = UIState

packAnyHasUIToUIState :: AnyHasUI -> UIState
packAnyHasUIToUIState (AnyHasUI a) = packUIState a (getUI a)

app :: App UIState Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

drawUI :: UIState -> [Widget Name]
drawUI (UIState s ui) = case UI.baseLayout ui of
  UI.GameUI desc -> drawGameUI desc
  UI.InventoryUI desc -> undefined
  UI.ListMenuUI desc -> drawMenu desc
  UI.End -> [C.center $ str "game stopped"]

drawGameUI :: GameUI.UIDesc a b -> [Widget n]
drawGameUI desc =
  [ (drawMap (GameUI.getMap desc) <=> drawLog (GameUI.getLog desc))
      <+> ( drawStats (GameUI.getStats desc)
              <=> drawItemMenu (GameUI.getItemMenu desc)
          )
  ]

drawMap :: GameUI.Map -> Widget n
drawMap m =
  withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Snake") $ vBox rows
  where
    rows = map str (m ^. GameUI.mapField)

drawLog :: GameUI.Log -> Widget n
drawLog l = vBox rows
  where
    rows = [str s | (i, s) <- zip [0 ..] (l ^. GameUI.logRecords)]

drawStats :: GameUI.Stats -> Widget n
drawStats _ = fill '#'

drawItemMenu :: GameUI.ItemMenu a b -> Widget n
drawItemMenu _ = fill '$'

drawMenu :: ListMenu.UIDesc a b -> [Widget n]
drawMenu menu = [vBox rows]
  where
    rows =
      [ C.center
          $ str
          $ if menu ^. ListMenu.selectedItem == Just i
            then s ++ "+"
            else s
        | (i, (ListMenu.ListItem s _)) <- zip [0 ..] (menu ^. ListMenu.items)
      ]

handleEvent ::
  UIState ->
  BrickEvent Name Tick ->
  EventM Name (Next UIState)
handleEvent (UIState s ui) event = case UI.baseLayout ui of
  UI.GameUI desc -> case event of
    VtyEvent e -> dispatchVtyEventGameUI s ui e desc
    _ -> continue (UIState s ui)
  UI.InventoryUI desc -> case event of
    VtyEvent e -> undefined 
    _ -> continue (UIState s ui)
  UI.ListMenuUI desc -> case event of
    VtyEvent e -> dispatchVtyEventListMenuUI s ui e desc
    _ -> continue $ UIState s ui
  UI.End -> halt $ UIState s ui

dispatchVtyEventGameUI ::
  (HasUI a) =>
  a ->
  UI a ->
  V.Event ->
  GameUI.UIDesc a AnyHasUI ->
  EventM n (Next UIState)
dispatchVtyEventGameUI state ui event desc = case event of
  V.EvKey V.KUp [] -> continue $ tryArrowPress Keys.Up
  V.EvKey V.KDown [] -> continue $ tryArrowPress Keys.Down
  V.EvKey V.KRight [] -> continue $ tryArrowPress Keys.Right
  V.EvKey V.KLeft [] -> continue $ tryArrowPress Keys.Left
  V.EvKey (V.KChar 'q') [] -> continue $ tryKeyPress (Keys.Letter 'q')
  _ -> continue $ packedS
  where
    packedS = packUIState state ui
    onKeyPress = desc ^. GameUI.onKeyPress
    onArrowPress = desc ^. GameUI.onArrowPress
    tryArrowPress key = fromMaybe packedS $
      do
        f <- onArrowPress
        let newState = f key state
        return $ packAnyHasUIToUIState newState
    tryKeyPress key = fromMaybe packedS $
      do
        f <- onKeyPress
        let newState = f key state
        return $ packAnyHasUIToUIState newState

dispatchVtyEventInventoryUI state ui event desc = undefined

dispatchVtyEventListMenuUI ::
  (HasUI s) =>
  s ->
  UI s ->
  V.Event ->
  ListMenu.UIDesc s AnyHasUI ->
  EventM n (Next UIState)
dispatchVtyEventListMenuUI state ui event desc = case event of
  V.EvKey V.KUp [] ->
    continue $
      packUIState
        state
        (UI.UIDesc . UI.ListMenuUI $ ListMenu.moveSelectionUp desc)
  V.EvKey V.KDown [] ->
    continue $
      packUIState
        state
        (UI.UIDesc . UI.ListMenuUI $ ListMenu.moveSelectionDown desc)
  V.EvKey V.KEnter [] -> continue $ tryClick
  _ -> continue $ packedS
  where
    packedS = packUIState state ui
    clickItem = ListMenu.clickItem desc
    tryClick = fromMaybe packedS $
      do
        f <- clickItem
        let newState = f state
        return $ packAnyHasUIToUIState newState

theMap :: AttrMap
theMap = attrMap V.defAttr []
