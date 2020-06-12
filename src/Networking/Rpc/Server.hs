{-# LANGUAGE TypeApplications #-}
{-# language FlexibleContexts      #-}
{-# language PartialTypeSignatures #-}
{-# language OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# language DataKinds             #-}
module Networking.Rpc.Server
  ( RpcServer
  , roguelikeServer
  ) where

import qualified Networking.Rpc.Schema as S
import Mu.Server (MonadServer, SingleServerT)
import Data.Functor (($>))

import Mu.GRpc.Server
import Mu.Server

class MonadServer m => RpcServer m where
  -- |Returns list of sessions running on this server.
  -- (argument is a useless stub, needed for grpc)
  getSessions :: m S.SessionsList

  -- |Returns session state by id.
  -- If no session has such an id, returns Nothing as "state" field
  getSessionState :: S.SessionId -> m S.SessionState

  -- |Applies an action made by a player
  makeAction :: S.ActionRequest -> m ()

  -- |Idk, see Game.Server functions
  clickSlot :: S.PlayerClickSlotRequest -> m ()

  -- |Idk, see Game.Server functions
  clickItem :: S.PlayerClickItemRequest -> m ()

  -- |Idk, see Game.Server functions
  createNewSession :: m S.SessionId

  -- |Idk, see Game.Server functions
  addNewPlayerToSession :: S.SessionId -> m S.PlayerId

  -- |Idk, see Game.Server functions
  removePlayerFromSession :: S.RemovePlayerRequest -> m ()

-- |Server for the game protobuf service
roguelikeServer :: RpcServer m => SingleServerT S.ServerServicer m _
roguelikeServer = singleService
  ( method @"getSessions" getSessions_
  , method @"getSessionState" getSessionState_
  , method @"makeAction" makeAction_
  , method @"clickSlot" clickSlot_
  , method @"clickItem" clickItem_
  , method @"createNewSession" createNewSession_
  , method @"addNewPlayerToSession" addNewPlayerToSession_
  , method @"removePlayerFromSession" removePlayerFromSession_
  ) where
  getSessions_ :: RpcServer m => S.Empty -> m S.SessionsList
  getSessions_ = const getSessions

  getSessionState_ :: RpcServer m => S.SessionId -> m S.SessionState
  getSessionState_ = getSessionState

  makeAction_ :: RpcServer m => S.ActionRequest -> m S.Empty
  makeAction_ action = makeAction action $> S.Empty

  clickSlot_ :: RpcServer m => S.PlayerClickSlotRequest -> m S.Empty
  clickSlot_ = ($> S.Empty) . clickSlot

  clickItem_ :: RpcServer m => S.PlayerClickItemRequest -> m S.Empty
  clickItem_ = ($> S.Empty) . clickItem

  createNewSession_ :: RpcServer m => S.Empty -> m S.SessionId
  createNewSession_ _ = createNewSession

  addNewPlayerToSession_ :: RpcServer m => S.SessionId -> m S.PlayerId
  addNewPlayerToSession_ = addNewPlayerToSession

  removePlayerFromSession_ :: RpcServer m => S.RemovePlayerRequest -> m S.Empty
  removePlayerFromSession_ = ($> S.Empty) . removePlayerFromSession
