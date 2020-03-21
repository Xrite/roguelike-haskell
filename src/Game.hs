{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Game where

import           Control.Monad.State
import           Game.Environment
import           Game.IO.GameIO
import           Game.GameLevels.GameLevel
import           Game.GameLevels.MapCell
import           UI.Descriptions.GameUIDesc
import qualified UI.Descriptions.ListMenuDesc as ListMenu
import           Game.Controller.Moves
import           Game.Unit.Action
import           UI.UI
import           UI.Keys as Keys
import           Control.Lens
import           Data.Maybe
import           Game.Environment (makeEnvironment, Environment, UnitId
                                 , makeUnitId, unitById)
import           Game.GameLevels.GenerateLevel (testGameLevel)
import           Game.Item (createWeapon)
import           Game.Unit.Inventory (emptyInventory)
import           Game.Unit.Mob
import           Game.Unit.Player (makePlayer, Player)
import           Game.Unit.Stats as Stats
import           Game.Unit.Unit
import           Game.Unit.TimedEffects (empty)
import           Game.Unit.Unit (createUnitData, packUnit, UnitData, asUnitData)
import           Data.Maybe (isNothing, isJust)


import Game.Unit.Action





type GameUI = UI GameState

data GameState = Game { __env :: Environment }
               | MainMenu GameUI
               | EndState

makeLenses ''GameState

instance HasUI GameState where
  currentUI (Game env) = gameUI env
  currentUI (MainMenu ui) = ui
  currentUI EndState = terminalUI

gameUI :: Environment -> GameUI
gameUI gameEnv = makeGameUI
  $ do
    let level = getCurrentLevel gameEnv
    setMap $ renderLevel level (unitById (playerId gameEnv) gameEnv)
    setArrowPress arrowPress
    setKeyPress keyPress
  where
    arrowPress :: Arrows -> GameState -> GameState
    arrowPress Keys.Up (Game env) = Game . fromMaybe env
      $ maybeMakeAction (playerId env) moveUp env
    arrowPress Keys.Down (Game env) = Game . fromMaybe env
      $ maybeMakeAction (playerId env) moveDown env
    arrowPress Keys.Left (Game env) = Game . fromMaybe env
      $ maybeMakeAction (playerId env) moveLeft env
    arrowPress Keys.Right (Game env) = Game . fromMaybe env
      $ maybeMakeAction (playerId env) moveRight env
    
    keyPress :: Keys.Keys -> GameState -> GameState
    keyPress (Keys.Letter 'q') (Game env) = MainMenu mainMenuUI

renderLevel :: GameLevel -> AnyUnit -> [String]
renderLevel lvl player = [[if px == x && py == y then 'A' else renderCell $ getCell (x, y) mp | x <- [xFrom .. xTo]]
                  | y <- [yFrom .. yTo]]
  where
    mp = lvl ^. lvlMap
    (px, py) = getPosition player
    ((xFrom, yFrom), (xTo, yTo)) = getMapSize mp

mainMenuUI :: GameUI
mainMenuUI = makeMenuUI
  $ do
    ListMenu.setTitle "Main menu"
    ListMenu.addItem "random" id
    ListMenu.addItem "load level" id
    ListMenu.addItem "test level" (const (Game testEnvironment))
    ListMenu.addItem "quit" (const EndState)
    ListMenu.selectItem 0


ourPlayer = makeSomePlayer $ makeUnitData (5, 6)

testEnvironment :: Environment
testEnvironment =
  makeEnvironment
    ourPlayer
    [ packUnit $ Mob $ makeUnitData (7, 8)
    , packUnit $ Mob $ makeUnitData (14, 15)
    , packUnit $ Mob $ makeUnitData (4, 6)
    , packUnit $ Mob $ makeUnitData (5, 6)
    ]
    [testGameLevel]
    0

makeUnitData :: (Int, Int) -> UnitData
makeUnitData position =
  createUnitData
    position
    0
    (Stats.Stats 10 10 10 1)
    empty
    emptyInventory
    (createWeapon "weapon" (return ()) 'A')
    undefined

makeSomePlayer :: UnitData -> Player
makeSomePlayer unitData = makePlayer unitData