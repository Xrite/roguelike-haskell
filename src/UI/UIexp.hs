{-# LANGUAGE MultiParamTypeClasses #-}
module UI.UIexp where

import           Control.Lens
import           Control.Applicative
import           Data.Maybe (fromMaybe)
import Game.Environment (Environment, UnitId(..), getCurrentLevel, unitById, _player)
import Game.GameLevels.GameLevel
import Game.Unit.Stats (Stats)
import UI.Keys as Keys
import Game.Controller.Moves (maybeMakeAction)
import Game.Unit.Action
import Game.Unit.Unit (asUnitData, _stats, UnitData)
import UI.Descriptions.GameUIDesc
import Game.GameLevels.MapCell (renderCell)
import Game.Unit.Player (Player, getPosition)
import UI.TestEnvironment

arrowPress :: Arrows -> Environment -> Environment
arrowPress Keys.Up env = fromMaybe env $ maybeMakeAction (UnitId 0) (Move Positive Zero) env
arrowPress Keys.Down env = fromMaybe env $ maybeMakeAction (UnitId 0) (Move Negative Zero) env
arrowPress Keys.Left env = fromMaybe env $ maybeMakeAction (UnitId 0) (Move Zero Positive) env
arrowPress Keys.Right env = fromMaybe env $ maybeMakeAction (UnitId 0) (Move Zero Negative) env
arrowPress Keys.UpRight env = fromMaybe env $ maybeMakeAction (UnitId 0) (Move Positive Negative) env
arrowPress Keys.DownRight env = fromMaybe env $ maybeMakeAction (UnitId 0) (Move Negative Negative) env
arrowPress Keys.UpLeft env = fromMaybe env $ maybeMakeAction (UnitId 0) (Move Positive Positive) env
arrowPress Keys.DownLeft env = fromMaybe env $ maybeMakeAction (UnitId 0) (Move Negative Positive) env

class UIexp a where
  keyPress :: Keys -> a -> a
  render :: a -> UIDesc

instance UIexp GameState where
  keyPress key (Game env) = Game $ fromMaybe env $ do
      arrow <- parseArrows key
      return $ arrowPress arrow env
  keyPress (Keys.Letter 't') MainMenu = Game testEnvironment
  keyPress _ state = state

  render MainMenu = MenuUI $ ListMenuUIDesc (Title "Main menu") [ListItem "(r) Random", ListItem "(l) Load", ListItem "(t) Test environment"]
  render (Game env) = GameUI $ GameUIDesc (renderLevel (getCurrentLevel env) (_player env)) (Log ["You've entered the game"])

renderLevel :: GameLevel -> Player -> MapDesc
renderLevel lvl pl = MapDesc
  [[if x == px && y == py then 'a' else renderCell $  getCell (x, y) mp | x <- [xFrom .. xTo]] | y <- [yFrom .. yTo]]
  where
    mp = lvl ^. lvlMap
    ((xFrom, yFrom), (xTo, yTo)) = getMapSize mp
    px = fst $ getPosition pl
    py = snd $ getPosition pl
