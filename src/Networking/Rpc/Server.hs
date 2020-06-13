{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# language FlexibleContexts      #-}
{-# language PartialTypeSignatures #-}
{-# language OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# language DataKinds             #-}
module Networking.Rpc.Server
  ( RpcServer (..)
  , roguelikeServer
  ) where

import qualified Networking.Rpc.Schema as S
import Mu.Server (MonadServer, SingleServerT)
import Data.Functor (($>))

import Mu.GRpc.Server
import Mu.Server

class MonadServer m => RpcServer m s where
  -- |Returns list of sessions running on this server.
  -- (argument is a useless stub, needed for grpc)
  getSessions :: s -> m S.SessionsList

  -- |Returns session state by id.
  -- If no session has such an id, returns Nothing as "state" field
  getSessionState :: s -> S.SessionId -> m S.SessionState

  -- |Applies an action made by a player
  makeAction :: s -> S.ActionRequest -> m ()

  -- |Idk, see Game.Server functions
  clickSlot :: s -> S.PlayerClickSlotRequest -> m ()

  -- |Idk, see Game.Server functions
  clickItem :: s -> S.PlayerClickItemRequest -> m ()

  -- |Idk, see Game.Server functions
  createNewSession :: s -> m S.SessionId

  -- |Idk, see Game.Server functions
  addNewPlayerToSession :: s -> S.SessionId -> m S.PlayerId

  -- |Idk, see Game.Server functions
  removePlayerFromSession :: s -> S.RemovePlayerRequest -> m ()

-- |Server for the game protobuf service
roguelikeServer :: RpcServer m s => s -> SingleServerT S.ServerServicer m _
roguelikeServer cfg = singleService
  ( method @"getSessions" $ getSessions_ cfg
  , method @"getSessionState" $ getSessionState_ cfg
  , method @"makeAction" $ makeAction_ cfg
  , method @"clickSlot" $ clickSlot_ cfg
  , method @"clickItem" $ clickItem_ cfg
  , method @"createNewSession" $ createNewSession_ cfg
  , method @"addNewPlayerToSession" $ addNewPlayerToSession_ cfg
  , method @"removePlayerFromSession" $ removePlayerFromSession_ cfg
  ) where
  getSessions_ :: RpcServer m s => s -> S.Empty -> m S.SessionsList
  getSessions_ cfg = const (getSessions cfg)

  getSessionState_ :: RpcServer m s => s -> S.SessionId -> m S.SessionState
  getSessionState_ cfg = getSessionState cfg

  makeAction_ :: RpcServer m s => s -> S.ActionRequest -> m S.Empty
  makeAction_ cfg action = makeAction cfg action $> S.Empty

  clickSlot_ :: RpcServer m s => s -> S.PlayerClickSlotRequest -> m S.Empty
  clickSlot_ cfg = ($> S.Empty) . clickSlot cfg 

  clickItem_ :: RpcServer m s => s -> S.PlayerClickItemRequest -> m S.Empty
  clickItem_ cfg = ($> S.Empty) . clickItem cfg

  createNewSession_ :: RpcServer m s => s -> S.Empty -> m S.SessionId
  createNewSession_ cfg _ = createNewSession cfg

  addNewPlayerToSession_ :: RpcServer m s => s -> S.SessionId -> m S.PlayerId
  addNewPlayerToSession_ cfg = addNewPlayerToSession cfg

  removePlayerFromSession_ :: RpcServer m s => s -> S.RemovePlayerRequest -> m S.Empty
  removePlayerFromSession_ cfg = ($> S.Empty) . removePlayerFromSession cfg
