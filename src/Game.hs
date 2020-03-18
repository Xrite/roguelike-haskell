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
import           Game.Controller.Moves
import           Game.Unit.Action
import           UI.UI
import           UI.Keys as Keys
import           Control.Lens
import           Data.Maybe



data GameState = Game { __env :: Environment }
               | MainMenu (UI GameState GameState)


makeLenses ''GameEnv

instance HasUI GameState where
    currentUI Game env = gameUI

gameUI :: Environment -> UI GameState GameState
gameUI gameEnv = makeGameUI . mkGameUI
  $ do
    let level = getCurrentLevel $ gameEnv ^. _env
    setMap $ renderLevel level
    setArrowPress arrowPress
  where
    arrowPress :: Arrows -> GameState -> GameState
    arrowPress Keys.Up (GameEnv env) = GameEnv . fromMaybe env
      $ maybeMakeAction (UnitId 0) (Move Positive Zero) env
    arrowPress Keys.Down (GameEnv env) = GameEnv . fromMaybe env
      $ maybeMakeAction (UnitId 0) (Move Negative Zero) env
    arrowPress Keys.Left (GameEnv env) = GameEnv . fromMaybe env
      $ maybeMakeAction (UnitId 0) (Move Zero Positive) env
    arrowPress Keys.Right (GameEnv env) = GameEnv . fromMaybe env
      $ maybeMakeAction (UnitId 0) (Move Zero Negative) env


renderLevel :: GameLevel -> [String]
renderLevel lvl =
  [[renderCell $ getCell (x, y) mp | x <- [xFrom .. xTo]] | y <- [yFrom .. yTo]]
  where
    mp = lvl ^. lvlMap
    ((xFrom, yFrom), (xTo, yTo)) = getMapSize mp
