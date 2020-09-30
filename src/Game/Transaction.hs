module Game.Transaction (Transaction, applyTransaction, unitAction, clickSlot, clickItem) where

import Control.Monad.Except (runExceptT)
import Control.Monad.State (gets)
import Game.Environment
import Game.Modifiers.UnitOp as UnitOp
import Game.Unit (Action)
import Debug.Trace

data Transaction = MkTransaction [TransactionAtom]

data TransactionAtom
  = UnitAction PlayerId Action
  | ClickSlot PlayerId Int
  | ClickItem PlayerId Int
  | AddPlayer PlayerId
  | RemovePlayer PlayerId

unitAction :: PlayerId -> Action -> Transaction
unitAction pid a = MkTransaction [UnitAction pid a]

clickSlot :: PlayerId -> Int -> Transaction
clickSlot pid i = MkTransaction [ClickSlot pid i]

clickItem :: PlayerId -> Int -> Transaction
clickItem pid i = MkTransaction [ClickItem pid i]

makeTurn :: PlayerId -> Action -> GameEnv ()
makeTurn pid playerAction = do
  _ <- runExceptT $ updateSeenByPlayer pid
  _ <- runExceptT (evalAction pid playerAction)
  mobs <- gets getActiveMobs
  eitherActions <- mapM (\u -> gets (getAction u)) mobs
  _ <- runExceptT $ case sequence eitherActions of
    Left _ -> error "failed to get mobs action in makeTurn"
    Right as -> sequence as
  units <- gets getActiveUnits
  _ <- runExceptT $ traverse (`affectUnit` UnitOp.tickTimedEffects) units
  _ <- runExceptT $ updateSeenByPlayer pid
  return ()

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

applyTransactionAtom :: TransactionAtom -> GameEnv ()
applyTransactionAtom ta = case ta of
  UnitAction pid a -> makeTurn pid a
  ClickSlot pid i -> doClickSlot pid i
  ClickItem pid i -> doClickItem pid i
  _ -> return ()

applyTransaction :: Transaction -> GameEnv ()
applyTransaction (MkTransaction ts) = mapM_ applyTransactionAtom ts
