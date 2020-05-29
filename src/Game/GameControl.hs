{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
module Game.GameControl where

import Game.Unit.Action
import Game.Environment (Environment, runGameEnv, PlayerId)
import Control.Monad.Free
import Game.Transaction as Transaction

data GameF next where
    MakeAction :: Action -> next -> GameF next
    ClickSlot :: Int -> next -> GameF next
    ClickItem :: Int -> next -> GameF next
    deriving (Functor)

type GameL = Free GameF

makeAction :: MonadFree GameF m => Action -> m ()
makeAction action = liftF $ MakeAction action ()

clickSlot :: MonadFree GameF m => Int -> m ()
clickSlot slot = liftF $ ClickSlot slot ()

clickItem :: MonadFree GameF m => Int -> m ()
clickItem item = liftF $ ClickItem item ()

data GameCfg
  = GameCfg
      { handleAction :: Action -> IO (),
        handleClickSlot :: Int -> IO (),
        handleClickItem :: Int -> IO (),
        playerId :: PlayerId
      }

interpret :: GameCfg -> Environment -> GameL a -> IO Environment
interpret cfg env (Free x) = case x of
    MakeAction a next -> do
        (handleAction cfg) a
        let env' = snd $ runGameEnv (applyTransaction (Transaction.unitAction pid a)) env
        interpret cfg env' next
    ClickSlot i next -> do 
        (handleClickSlot cfg) i
        let env' = snd $ runGameEnv (applyTransaction (Transaction.clickSlot pid i)) env
        interpret cfg env' next
    ClickItem i next -> do
        (handleClickItem cfg) i
        let env' = snd $ runGameEnv (applyTransaction (Transaction.clickItem pid i)) env
        interpret cfg env' next
    where
        pid = playerId cfg
interpret _ env (Pure _) = return env

