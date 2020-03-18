module UI.BrickUI where

import           UI.UIexp as UI
import           UI.Descriptions.GameUIDesc
import           Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified UI.Keys as Keys
import qualified Graphics.Vty as V
import           Control.Lens
import           Data.Maybe


type Name = ()

data Tick = Tick

app :: (UIexp a) => App a Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

drawUI :: UIexp a => a -> [Widget Name]
drawUI ui = case UI.render ui of
  GameUI gameUIDesc -> drawGameUI gameUIDesc
  MenuUI listMenuUIDesc -> [drawItemMenu listMenuUIDesc]

drawGameUI :: GameUIDesc -> [Widget n]
drawGameUI desc =
  [ padRight (Pad 2) (drawMap (getMap desc))]


drawMap :: MapDesc -> Widget n
drawMap m = withBorderStyle BS.unicodeBold $
  B.borderWithLabel (str "Snake") $
  vBox rows
  where
    rows = map str (mapField m)

drawLog :: Log -> Widget n
drawLog l = vBox rows
  where
    rows = map str $ logRecords l


drawItemMenu :: ListMenuUIDesc -> Widget n
drawItemMenu menu = vBox rows
  where
    rows = map (\(ListItem s) -> str s) $ __items menu

handleEvent :: (UIexp a) => a -> BrickEvent Name Tick -> EventM Name (Next a)
handleEvent ui event = case event of
  (VtyEvent (V.EvKey (V.KChar 'q') [])) -> halt ui
  (VtyEvent (V.EvKey (V.KChar c) [])) -> continue $ keyPress (Keys.Letter c) ui
  _ -> continue ui


theMap :: AttrMap
theMap = attrMap V.defAttr []
