{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module UI.BrickUI where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens
import Control.Monad.IO.Class
import Data.Array
import Data.Maybe
import Debug.Trace
import qualified Graphics.Vty as V
import qualified UI.Descriptions.GameUIDesc as GameUI
import qualified UI.Descriptions.InventoryUIDesc as InventoryUI
import qualified UI.Descriptions.ListMenuDesc as ListMenu
import qualified UI.Descriptions.EnterDataUIDesc as EnterDataUI
import qualified UI.Keys as Keys
import UI.UI as UI

type Name = ()

data UIState m e = forall s. HasUI m s e => UIState s (UI m s e)

class Monad m => ToIO m where
  toIO :: m a -> IO a

instance ToIO IO where
  toIO = id

packUIState :: HasUI m s e => s -> UI m s e -> UIState m e
packUIState = UIState

packAnyHasIOUIToUIState :: AnyHasUI m e -> UIState m e
packAnyHasIOUIToUIState (AnyHasUI a) = packUIState a (getUI a)

app :: ToIO m => App (UIState m e) e Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

drawUI :: UIState m e -> [Widget Name]
drawUI (UIState s ui) = case UI.baseLayout ui of
  UI.GameUI desc -> drawGameUI desc
  UI.InventoryUI desc -> drawInventoryUI desc
  UI.ListMenuUI desc -> drawMenu desc
  UI.EnterDataUI desc -> drawEnterText desc
  UI.End -> [C.center $ str "game stopped"]

drawGameUI :: GameUI.UIDesc e a b -> [Widget n]
drawGameUI desc =
  [ C.center $
      (drawMap (desc ^. GameUI.map) <=> drawLog (desc ^. GameUI.log))
        <+> ( drawStats (desc ^. GameUI.stats)
                <=> drawEquippedItems (desc ^. GameUI.equippedItems)
            )
  ]

drawInventoryUI :: InventoryUI.UIDesc e a b-> [Widget n]
drawInventoryUI desc =
  [ C.center $ withBorderStyle BS.unicodeBold (B.borderWithLabel (str "Items") drawItems)
      <+> withBorderStyle BS.unicodeBold (B.borderWithLabel (str "Slots") drawSlots)
  ]
  where
    drawItems = vBox [drawItem i x | (i, x) <- zip [0 ..] $ desc ^. InventoryUI.items]
    drawItem i x
      | Just i == desc ^. InventoryUI.selectedItem, desc ^. InventoryUI.selectedFrame == InventoryUI.First = withAttr selectedAttr $ str x
      | otherwise = withAttr notSelectedAttr $ str x
    drawSlots = vBox [drawSlot i x | (i, x) <- zip [0 ..] $ desc ^. InventoryUI.slots]
    drawSlot i (name, item)
      | Just i == desc ^. InventoryUI.selectedItem, desc ^. InventoryUI.selectedFrame == InventoryUI.Second = withAttr selectedAttr $ slotStr name item
      | otherwise = withAttr notSelectedAttr $ slotStr name item
    slotStr name item = str $ name ++ ": " ++ item

drawMap :: GameUI.Map -> Widget n
drawMap m =
  withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Map")
    $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- reverse [yFrom .. yTo]]
    cellsInRow y = [drawCoord (x, y) | x <- [xFrom .. xTo]]
    ((xFrom, yFrom), (xTo, yTo)) = bounds $ m ^. GameUI.mapTerrain
    drawCoord c
      | not $ (m ^. GameUI.mapHasBeenSeenByPlayer) c = str " "
      | not $ (m ^. GameUI.mapIsVisibleToPlayer) c = charWithAttr shadowedAttr $ (m ^. GameUI.mapTerrain) ! c
      | c == (m ^. GameUI.mapPlayerPosition) = charWithAttr playerAttr $ m ^. GameUI.mapPlayerPortrait
      | Just p <- lookup c (m ^. GameUI.mapMobs) = charWithAttr visibleAttr p
      | Just i <- lookup c (m ^. GameUI.mapEntities) = charWithAttr visibleAttr i
      | Just i <- lookup c (m ^. GameUI.mapPlayers) = charWithAttr visibleAttr i
      | otherwise = charWithAttr visibleAttr $ (m ^. GameUI.mapTerrain) ! c

charWithAttr :: AttrName -> Char -> Widget n
charWithAttr attr ch = withAttr attr (str [ch])

drawLog :: GameUI.Log -> Widget n
drawLog l = vBox rows
  where
    rows = [str s | (i, s) <- zip [0 ..] (l ^. GameUI.logRecords)]

drawStats :: GameUI.Stats -> Widget n
drawStats s =
  withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Stats") $ vBox (map drawPair $ s ^. GameUI.statsRecords)
  where
    drawPair (key, value) = str (key ++ ": ") <+> str value

drawEquippedItems :: GameUI.EquippedItems -> Widget n
drawEquippedItems equippedItems =
  withBorderStyle BS.unicodeRounded $ B.borderWithLabel (str "Equipped items") $ vBox (map drawItem $ equippedItems ^. GameUI.equippedItemsSlots)
  where
    drawItem (slot, maybeName) = str (slot ++ ": ") <+> fromMaybe (str "free") (str <$> maybeName)

drawMenu :: ListMenu.UIDesc e a b -> [Widget n]
drawMenu menu = [vBox rows]
  where
    rows =
      [ C.center
          $ drawElem i s
        | (i, (ListMenu.ListItem s _)) <- zip [0 ..] (menu ^. ListMenu.items)
      ]
    drawElem i s
     | menu ^. ListMenu.selectedItem == Just i = withAttr selectedAttr $ str s
     | otherwise = withAttr notSelectedAttr $ str s

drawEnterText :: EnterDataUI.UIDesc e a b -> [Widget n]
drawEnterText enterText = [C.center $ drawTitle <=> drawInput]
  where
    drawTitle = str $ EnterDataUI.getTitle enterText
    drawInput = str $ EnterDataUI.getInsertedText enterText


handleEvent ::
  (ToIO m) =>
  UIState m e ->
  BrickEvent Name e ->
  EventM Name (Next (UIState m e))
handleEvent (UIState s ui) event = case UI.baseLayout ui of
  UI.GameUI desc -> case event of
    VtyEvent e -> dispatchVtyEventGameUI s ui e desc
    AppEvent e -> trace "got event in gameUI" $ dispatchCustomEventGameUI s ui e desc
    _ -> continue (UIState s ui)
  UI.InventoryUI desc -> case event of
    VtyEvent e -> dispatchVtyEventInventoryUI s ui e desc
    AppEvent e -> trace "got event in InventoryUI" $ dispatchCustomEventInventoryUI s ui e desc
    _ -> continue (UIState s ui)
  UI.ListMenuUI desc -> case event of
    VtyEvent e -> dispatchVtyEventListMenuUI s ui e desc
    _ -> continue $ UIState s ui
  UI.EnterDataUI desc -> case event of
    VtyEvent e -> dispatchVtyEventEnterDataUI s ui e desc
    _ -> continue $ UIState s ui
  UI.End -> halt $ UIState s ui

dispatchCustomEventGameUI ::
  (ToIO m, HasUI m s e) =>
  s ->
  UI m s e ->
  e ->
  GameUI.UIDesc e s (m (AnyHasUI m e))  ->
  EventM n (Next (UIState m e))
dispatchCustomEventGameUI state ui event desc = 
  case desc ^. GameUI.customEventHandler of
    Just f -> (liftIO . toIO $ packAnyHasIOUIToUIState <$> f event state) >>= continue
    Nothing -> continue $ packUIState state ui

dispatchVtyEventGameUI ::
  (ToIO m, HasUI m s e) =>
  s ->
  UI m s e ->
  V.Event ->
  GameUI.UIDesc e s (m (AnyHasUI m e))  ->
  EventM n (Next (UIState m e))
dispatchVtyEventGameUI state ui event desc =
  case event of
    V.EvKey V.KUp [] -> passArrow Keys.Up
    V.EvKey V.KDown [] -> passArrow Keys.Down
    V.EvKey V.KRight [] -> passArrow Keys.Right
    V.EvKey V.KLeft [] -> passArrow Keys.Left
    V.EvKey V.KUpLeft [] -> passArrow Keys.UpLeft
    V.EvKey V.KUpRight [] -> passArrow Keys.UpRight
    V.EvKey V.KDownLeft [] -> passArrow Keys.DownLeft
    V.EvKey V.KDownRight [] -> passArrow Keys.DownRight
    V.EvKey V.KCenter [] -> passArrow Keys.Center
    V.EvKey (V.KChar c) [] -> liftIO (toIO $ tryKeyPress (Keys.Letter c)) >>= continue
    _ -> continue packedS
  where
    passArrow k = liftIO (toIO $ tryArrowPress k) >>= continue
    packedS = packUIState state ui
    onKeyPress = desc ^. GameUI.onKeyPress
    onArrowPress = desc ^. GameUI.onArrowPress
    tryArrowPress key =
      case onArrowPress of
        Nothing -> return packedS
        Just f -> packAnyHasIOUIToUIState <$> f key state
    tryKeyPress key =
      case onKeyPress of
        Nothing -> return packedS
        Just f -> packAnyHasIOUIToUIState <$> f key state

dispatchCustomEventInventoryUI state ui event desc = 
  case desc ^. InventoryUI.customEventHandler of
    Just f -> (liftIO . toIO $ packAnyHasIOUIToUIState <$> f event state) >>= continue
    Nothing -> continue $ packUIState state ui

dispatchVtyEventInventoryUI ::
  (ToIO m, HasUI m s e) =>
  s ->
  UI m s e ->
  V.Event ->
  InventoryUI.UIDesc e s (m (AnyHasUI m e)) ->
  EventM n (Next (UIState m e))
dispatchVtyEventInventoryUI state ui event desc = case event of
  V.EvKey V.KEnter [] -> liftIO (toIO $ fromMaybe (return packedS) onClick) >>= continue
  V.EvKey V.KUp [] -> continue $ packUIState state (UI.UIDesc . UI.InventoryUI $ InventoryUI.moveSelectionUp desc)
  V.EvKey V.KDown [] -> continue $ packUIState state (UI.UIDesc . UI.InventoryUI $ InventoryUI.moveSelectionDown desc)
  V.EvKey V.KLeft [] -> continue $ packUIState state (UI.UIDesc . UI.InventoryUI $ InventoryUI.switchSelection desc)
  V.EvKey V.KRight [] -> continue $ packUIState state (UI.UIDesc . UI.InventoryUI $ InventoryUI.switchSelection desc)
  V.EvKey (V.KChar 'q') [] -> liftIO (toIO $ fromMaybe (return packedS) onClose) >>= continue
  _ -> continue $ packedS
  where
    packedS = packUIState state ui
    onClick = fmap packAnyHasIOUIToUIState <$> (InventoryUI.onSelected desc <*> (desc ^. InventoryUI.selectedItem) <*> pure state)
    onClose = fmap packAnyHasIOUIToUIState <$> (desc ^. InventoryUI.onClosed <*> pure state)

dispatchVtyEventListMenuUI ::
  (ToIO m, HasUI m s e) =>
  s ->
  UI m s e ->
  V.Event ->
  ListMenu.UIDesc e s (m (AnyHasUI m e)) ->
  EventM n (Next (UIState m e))
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
  V.EvKey V.KEnter [] -> liftIO (toIO tryClick) >>= continue
  _ -> continue $ packedS
  where
    packedS = packUIState state ui
    clickItem = ListMenu.clickItem desc
    tryClick = case clickItem of
      Nothing -> return packedS
      Just f -> packAnyHasIOUIToUIState <$> f state

dispatchVtyEventEnterDataUI ::
  (ToIO m, HasUI m s e) =>
  s ->
  UI m s e ->
  V.Event ->
  EnterDataUI.UIDesc e s (m (AnyHasUI m e)) ->
  EventM n (Next (UIState m e))
dispatchVtyEventEnterDataUI state ui event desc = case event of
  V.EvKey (V.KChar k) [] ->
    continue $
      packUIState
        state
        (UI.UIDesc . UI.EnterDataUI $ EnterDataUI.addChar desc k)
  V.EvKey (V.KDel) [] ->
    continue $
      packUIState
        state
        (UI.UIDesc . UI.EnterDataUI $ EnterDataUI.removeChar desc)
  V.EvKey V.KEnter [] -> liftIO (toIO $ fromMaybe (return packedS) onAccept) >>= continue
  V.EvKey V.KEsc [] ->liftIO (toIO $ fromMaybe (return packedS) onClose) >>= continue
  _ -> continue $ packedS
  where
    packedS = packUIState state ui
    onAccept = fmap packAnyHasIOUIToUIState <$> (desc ^. EnterDataUI.acceptInputHandler <*> pure (EnterDataUI.getInsertedText desc) <*> pure state)
    onClose = fmap packAnyHasIOUIToUIState <$> (desc ^. EnterDataUI.closeHandler <*> pure state)

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (visibleAttr, V.white `on` V.black),
      (shadowedAttr, V.white `on` V.brightBlack),
      (selectedAttr, V.black `on` V.white),
      (playerAttr, V.red `on` V.black)
    ]

visibleAttr, shadowedAttr :: AttrName
visibleAttr = "visibleAttr"
shadowedAttr = "shadowedAttr"

selectedAttr, notSelectedAttr :: AttrName
selectedAttr = "selectedAttr"
notSelectedAttr = "notSelectedAttr"

playerAttr :: AttrName
playerAttr = "playerAttr"
