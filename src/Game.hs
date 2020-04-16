{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Lens
import Control.Monad.State
import Data.Maybe
import Data.Maybe (isJust, isNothing)
import Game.Controller.Moves
import Game.Environment
import Game.Environment
  ( Environment,
    UnitId,
    makeEnvironment,
    makeUnitId,
    unitById,
  )
import Game.GameLevels.GameLevel
import Game.GameLevels.GenerateLevel (randomBSPGeneratedLevel, testGameLevel)
import Game.GameLevels.Generation.BSPGen (GeneratorParameters (..))
import qualified Game.GameLevels.Generation.GenerationUtil as GU
import Game.GameLevels.MapCell
import Game.IO.GameIO
import Game.Item (createWeapon)
import Game.Unit.Action
import Game.Unit.Action ()
import Game.Unit.Inventory (emptyInventory)
import Game.Unit.Mob
import Game.Unit.Player (Player, makePlayer)
import Game.Unit.Stats as Stats
import Game.Unit.TimedEffects (empty)
import Game.Unit.Unit (AnyUnit, UnitData, asUnitData, createUnitData, getPosition, packUnit)
import System.Random (mkStdGen)
import UI.Descriptions.GameUIDesc
import qualified UI.Descriptions.ListMenuDesc as ListMenu
import UI.Keys as Keys
import UI.UI

type GameUI = UI GameState

data GameState
  = Game {__env :: Environment}
  | MainMenu GameUI
  | EndState

makeLenses ''GameState

instance HasUI GameState where
  currentUI (Game env) = gameUI env
  currentUI (MainMenu ui) = ui
  currentUI EndState = terminalUI

gameUI :: Environment -> GameUI
gameUI gameEnv = makeGameUI $
  do
    let level = getCurrentLevel gameEnv
    setMap $ renderLevel level (unitById (playerId gameEnv) gameEnv)
    setArrowPress arrowPress
    setKeyPress keyPress
  where
    arrowPress :: Arrows -> GameState -> GameState
    arrowPress Keys.Up (Game env) =
      Game . fromMaybe env $
        maybeMakeAction (playerId env) moveUp env
    arrowPress Keys.Down (Game env) =
      Game . fromMaybe env $
        maybeMakeAction (playerId env) moveDown env
    arrowPress Keys.Left (Game env) =
      Game . fromMaybe env $
        maybeMakeAction (playerId env) moveLeft env
    arrowPress Keys.Right (Game env) =
      Game . fromMaybe env $
        maybeMakeAction (playerId env) moveRight env
    keyPress :: Keys.Keys -> GameState -> GameState
    keyPress (Keys.Letter 'q') (Game env) = MainMenu mainMenuUI

renderLevel :: GameLevel -> AnyUnit -> [String]
renderLevel lvl player =
  [ [if px == x && py == y then 'λ' else renderCell $ getCell (x, y) mp | x <- [xFrom .. xTo]]
    | y <- [yFrom .. yTo]
  ]
  where
    mp = lvl ^. lvlMap
    (px, py) = getPosition player
    ((xFrom, yFrom), (xTo, yTo)) = getMapSize mp

mainMenuUI :: GameUI
mainMenuUI = makeMenuUI $
  do
    ListMenu.setTitle "Main menu"
    ListMenu.addItem "random" (const (Game $ randomEnvironment 42)) -- TODO use random generator or at least ask user to input a seed
    ListMenu.addItem "load level" id
    ListMenu.addItem "test level" (const (Game testEnvironment))
    ListMenu.addItem "quit" (const EndState)
    ListMenu.selectItem 0

randomEnvironment :: Int -> Environment
randomEnvironment seed =
  makeEnvironment
    ourPlayer
    [packUnit $ Mob $ makeUnitData startCoord]
    [lvl]
  where
    lvl = fst $ randomBSPGeneratedLevel (GU.Space (GU.Coord 0 0) (GU.Coord 50 50)) (GeneratorParameters 10 1.7 5) $ mkStdGen seed
    startCoord = _entrance $ _lvlMap lvl
    ourPlayer = makeSomePlayer $ makeUnitData startCoord

testEnvironment :: Environment
testEnvironment =
  makeEnvironment
    ourPlayer
    [ packUnit $ Mob $ makeUnitData (7, 8),
      packUnit $ Mob $ makeUnitData (14, 15),
      packUnit $ Mob $ makeUnitData (4, 6),
      packUnit $ Mob $ makeUnitData (5, 6)
    ]
    [testGameLevel]
  where
    ourPlayer = makeSomePlayer $ makeUnitData (5, 6)

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