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

import qualified Networking.Rpc.Schema as Schema
import Mu.Server (MonadServer, SingleServerT)
import Data.Functor (($>))

import Mu.GRpc.Server
import Mu.Server

class MonadServer m => RpcServer m where
  -- |Returns list of sessions running on this server.
  -- (argument is a useless stub, needed for grpc)
  getSessions :: m Schema.SessionsList

  -- |Returns session state by id.
  -- If no session has such an id, returns Nothing as "state" field
  getSessionState :: Schema.SessionId -> m Schema.SessionState

  -- |Applies an action made by a player
  makeAction :: Schema.ActionRequest -> m ()

  -- |Idk, see Game.Server functions
  clickSlot :: Schema.PlayerClickSlotRequest -> m ()

  -- |Idk, see Game.Server functions
  clickItem :: Schema.PlayerClickItemRequest -> m ()

  -- |Idk, see Game.Server functions
  createNewSession :: m Schema.SessionId

  -- |Idk, see Game.Server functions
  addNewPlayerToSession :: Schema.SessionId -> m Schema.PlayerId

  -- |Idk, see Game.Server functions
  removePlayerFromSession :: Schema.RemovePlayerRequest -> m ()

-- |Server for the game protobuf service
roguelikeServer :: RpcServer m => SingleServerT Schema.ServerServicer m _
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
  getSessions_ :: RpcServer m => Schema.Empty -> m Schema.SessionsList
  getSessions_ = const getSessions

  getSessionState_ :: RpcServer m => Schema.SessionId -> m Schema.SessionState
  getSessionState_ = getSessionState

  makeAction_ :: RpcServer m => Schema.ActionRequest -> m Schema.Empty
  makeAction_ action = makeAction action $> Schema.Empty

  clickSlot_ :: RpcServer m => Schema.PlayerClickSlotRequest -> m Schema.Empty
  clickSlot_ = ($> Schema.Empty) . clickSlot

  clickItem_ :: RpcServer m => Schema.PlayerClickItemRequest -> m Schema.Empty
  clickItem_ = ($> Schema.Empty) . clickItem

  createNewSession_ :: RpcServer m => Schema.Empty -> m Schema.SessionId
  createNewSession_ _ = createNewSession

  addNewPlayerToSession_ :: RpcServer m => Schema.SessionId -> m Schema.PlayerId
  addNewPlayerToSession_ = addNewPlayerToSession

  removePlayerFromSession_ :: RpcServer m => Schema.RemovePlayerRequest -> m Schema.Empty
  removePlayerFromSession_ = ($> Schema.Empty) . removePlayerFromSession
