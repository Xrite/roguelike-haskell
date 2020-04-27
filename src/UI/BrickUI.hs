{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.BrickUI where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens
import Control.Monad.IO.Class
import Data.Array
import Data.Maybe
import qualified Graphics.Vty as V
import qualified UI.Descriptions.GameUIDesc as GameUI
import qualified UI.Descriptions.ListMenuDesc as ListMenu
import qualified UI.Keys as Keys
import UI.UI as UI
import Debug.Trace

type Name = ()

data Tick = Tick

data UIState m = forall s. HasUI m s => UIState s (UI m s)

class Monad m => ToIO m where
  toIO :: m a -> IO a

instance ToIO IO where
  toIO = id

packUIState :: HasUI m s => s -> UI m s -> UIState m
packUIState = UIState

packAnyHasIOUIToUIState :: AnyHasUI m -> UIState m
packAnyHasIOUIToUIState (AnyHasUI a) = packUIState a (getUI a)

app :: ToIO m => App (UIState m) Tick Name
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
  [ C.center $
      (drawMap (desc ^. GameUI.map) <=> drawLog (desc ^. GameUI.log))
        <+> ( drawStats (desc ^. GameUI.stats)
                <=> drawEquippedItems (desc ^. GameUI.equippedItems)
            )
  ]

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
      | c == (m ^. GameUI.mapPlayerPosition) = charWithAttr visibleAttr $ m ^. GameUI.mapPlayerPortrait
      | Just p <- lookup c (m ^. GameUI.mapMobs) = charWithAttr visibleAttr p
      | Just i <- lookup c (m ^. GameUI.mapEntities) = charWithAttr visibleAttr i
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
  (ToIO m) =>
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
  (ToIO m, HasUI m a) =>
  a ->
  UI m a ->
  V.Event ->
  GameUI.UIDesc a (m (AnyHasUI m)) ->
  EventM n (Next (UIState m))
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

dispatchVtyEventInventoryUI state ui event desc = undefined

dispatchVtyEventListMenuUI ::
  (ToIO m, HasUI m s) =>
  s ->
  UI m s ->
  V.Event ->
  ListMenu.UIDesc s (m (AnyHasUI m)) ->
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
  V.EvKey V.KEnter [] -> liftIO (toIO tryClick) >>= continue
  _ -> continue $ packedS
  where
    packedS = packUIState state ui
    clickItem = ListMenu.clickItem desc
    tryClick = case clickItem of
      Nothing -> return packedS
      Just f -> packAnyHasIOUIToUIState <$> f state

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (visibleAttr, V.white `on` V.black),
      (shadowedAttr, V.white `on` V.brightBlack)
    ]

visibleAttr, shadowedAttr :: AttrName
visibleAttr = "visibleAttr"
shadowedAttr = "shadowedAttr"
