{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Game.GameControl where

import Control.Monad
import Control.Monad.Except (runExceptT)
import Control.Monad.Free
import Control.Monad.State (gets)
import Debug.Trace
import Game.Environment (Environment, PlayerId, runGameEnv)
import Game.Environment
import Game.Modifiers.UnitOp as UnitOp
import Game.Transaction as Transaction
import Game.Unit (Action)
import Game.Unit.Action
import Debug.Trace

doClickSlot :: PlayerId -> Int -> GameEnv ()
doClickSlot pid i
  | i == 0 = unitFreeHeadSlot pid
  | i == 1 = unitFreeChestSlot pid
  | i == 2 = unitFreeLegsSlot pid
  | i == 3 = unitFreeHandSlot pid
  | otherwise = return ()

doClickItem :: PlayerId -> Int -> GameEnv ()
doClickItem pid i = do
  _ <- runExceptT $ unitEquipItem pid i
  return ()

makeTurn :: PlayerId -> Action -> GameEnv ()
makeTurn pid action = do
  mobTurns
  makePlayerTurn pid action
  mobTurns

mobTurns :: GameEnv ()
mobTurns = do
  mUnit <- gets getCurrentUnit
  case mUnit of
    Nothing -> return ()
    Just unit -> do
      case downcast unit of
        Nothing -> return ()
        Just mid -> do
          popCurrentUnit
          makeMobTurn mid
          mobAlive <- gets (isUnitAlive mid)
          playerAlive <- not . null <$> gets (getActivePlayers)
          when (playerAlive && mobAlive) $ addUnitToQueue mid
          mobTurns

makePlayerTurn :: PlayerId -> Action -> GameEnv ()
makePlayerTurn pid action = do
  u <- gets getCurrentUnit
  when (Just (cast pid) == u) $ do
    popCurrentUnit
    _ <- runExceptT $ evalAction pid action
    _ <- runExceptT $ affectUnit pid UnitOp.tickTimedEffects
    _ <- runExceptT $ updateSeenByPlayer pid
    cleanupQueue
    shouldAddToQueue <- gets (isUnitAlive pid)
    when shouldAddToQueue $ addUnitToQueue pid

makeMobTurn :: MobId -> GameEnv ()
makeMobTurn mid = do
  runMob mid
  _ <- runExceptT $ affectUnit mid UnitOp.tickTimedEffects
  cleanupQueue
  where
    runMob mid = do
      eAction <- gets (getAction mid)
      case eAction of
        Left _ -> return ()
        Right action -> tryEvalAction mid action
    tryEvalAction mid cmd = do
      eAction <- runExceptT cmd
      case eAction of
        Left _ -> return ()
        Right action -> do
          _ <- runExceptT $ evalAction mid action
          return ()
