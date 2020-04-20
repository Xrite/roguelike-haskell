{-# LANGUAGE ExistentialQuantification #-}

module UI.BrickUI where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens
import Control.Monad.IO.Class
import Data.Maybe
import qualified Graphics.Vty as V
import qualified UI.Descriptions.GameUIDesc as GameUI
import qualified UI.Descriptions.ListMenuDesc as ListMenu
import qualified UI.Keys as Keys
import UI.UI as UI

type Name = ()

data Tick = Tick

data UIState m = forall s. HasIOUI m s => UIState s (UI m s)

packUIState :: HasIOUI m s => s -> UI m s -> UIState m
packUIState = UIState

packAnyHasIOUIToUIState :: AnyHasIOUI m -> UIState m
packAnyHasIOUIToUIState (AnyHasIOUI a) = packUIState a (getUI a)

app :: App (UIState m) Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

drawUI :: UIState m -> [Widget Name]
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
  UIState m ->
  BrickEvent Name Tick ->
  EventM Name (Next (UIState m))
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
  (HasIOUI m a) =>
  a ->
  UI m a ->
  V.Event ->
  GameUI.UIDesc a (m (AnyHasIOUI m)) ->
  EventM n (Next (UIState m))
dispatchVtyEventGameUI state ui event desc = case event of
  V.EvKey V.KUp [] -> liftIO (tryArrowPress Keys.Up) >>= continue
  V.EvKey V.KDown [] -> liftIO (tryArrowPress Keys.Down) >>= continue
  V.EvKey V.KRight [] -> liftIO (tryArrowPress Keys.Right) >>= continue
  V.EvKey V.KLeft [] -> liftIO (tryArrowPress Keys.Left) >>= continue
  V.EvKey (V.KChar 'q') [] -> liftIO (tryKeyPress (Keys.Letter 'q')) >>= continue
  _ -> continue $ packedS
  where
    packedS = packUIState state ui
    onKeyPress = desc ^. GameUI.onKeyPress
    onArrowPress = desc ^. GameUI.onArrowPress
    tryArrowPress key = case onArrowPress of
      Nothing -> return packedS
      Just f -> packAnyHasIOUIToUIState <$> f key state
    tryKeyPress key = case onKeyPress of
      Nothing -> return packedS
      Just f -> packAnyHasIOUIToUIState <$> f key state

dispatchVtyEventInventoryUI state ui event desc = undefined

dispatchVtyEventListMenuUI ::
  (HasIOUI m s) =>
  s ->
  UI m s ->
  V.Event ->
  ListMenu.UIDesc s (m (AnyHasIOUI m)) ->
  EventM n (Next (UIState m))
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
  V.EvKey V.KEnter [] -> liftIO tryClick >>= continue
  _ -> continue $ packedS
  where
    packedS = packUIState state ui
    clickItem = ListMenu.clickItem desc
    tryClick = case clickItem of
      Nothing -> return packedS
      Just f -> packAnyHasIOUIToUIState <$> f state

theMap :: AttrMap
theMap = attrMap V.defAttr []
