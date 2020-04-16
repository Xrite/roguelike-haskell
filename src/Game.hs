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
import           Game.GameLevels.GenerateLevel (testGameLevel, randomBSPGeneratedLevel)
import           Game.Item (createWeapon)
import           Game.Unit.Inventory (emptyInventory)
import           Game.Unit.Mob
import           Game.Unit.Player (makePlayer, Player)
import           Game.Unit.Stats as Stats
import           Game.Unit.TimedModifiers (empty)
import           Game.Unit.Unit (createUnitData, packUnit, UnitData, asUnitData, AnyUnit, getPosition)
import           Data.Maybe (isNothing, isJust)
import qualified Game.GameLevels.Generation.GenerationUtil as GU
import           Game.Unit.Action ()
import           System.Random (mkStdGen)
import           Game.GameLevels.Generation.BSPGen (GeneratorParameters(..))
import           Game.Modifiers.Modifier (modifyStats)
import Game.Modifiers.ModifierFactory (makeModifierFactory)
import qualified Data.Map as Map (empty)
import Game.Modifiers.EffectDesc (effectAtom)
import Game.Modifiers.EffectAtom





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
    setMap $ renderEnvironment gameEnv
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

mainMenuUI :: GameUI
mainMenuUI = makeMenuUI
  $ do
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
    [ packUnit ourPlayer ]
    [lvl]
    (makeModifierFactory Map.empty)
   where
    lvl = fst $ randomBSPGeneratedLevel (GU.Space (GU.Coord 0 0) (GU.Coord 50 50)) (GeneratorParameters 10 1.7 5) $ mkStdGen seed
    startCoord = _entrance $ _lvlMap lvl
    ourPlayer = makeSomePlayer $ makeUnitData startCoord 'λ'

testEnvironment :: Environment
testEnvironment =
  makeEnvironment
    ourPlayer
    [ packUnit ourPlayer --Mob $ makeUnitData (7, 8) 'U'
    , packUnit $ Mob $ makeUnitData (14, 15) 'U'
    , packUnit $ Mob $ makeUnitData (4, 6) 'U'
    , packUnit $ Mob $ makeUnitData (5, 6) 'U'
    ]
    [testGameLevel]
    (makeModifierFactory Map.empty)
  where
    ourPlayer = makeSomePlayer $ makeUnitData (7, 9) 'λ'

makeUnitData :: (Int, Int) -> Char -> UnitData
makeUnitData position render =
  createUnitData
    position
    0
    (Stats.Stats 10 10 10 1)
    empty
    emptyInventory
    (createWeapon "weapon" (effectAtom $ Damage 5) 'A')
    render
    undefined

makeSomePlayer :: UnitData -> Player
makeSomePlayer = makePlayer