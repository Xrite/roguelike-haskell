{-# language DataKinds           #-}
{-# language OverloadedLabels    #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications    #-}


module Networking.Rpc.Client where

import qualified Networking.Rpc.Schema as S
import Networking.Rpc.Schema
import Mu.Quasi.GRpc
import Mu.GRpc.Client.TyApps

import Network.Socket (HostName, PortNumber)
import Network.HTTP2.Client.Exceptions (ClientError)
import Mu.GRpc.Client.Optics (GRpcConnection)

-- |Builds a configuration of server connection.
-- Despite fancy names, takes string as an IP and number as port
setupClient :: HostName -> PortNumber -> IO (Either ClientError GrpcClient)
setupClient ip port = setupGrpcClient' $ grpcClientConfigSimple ip port False

getSessions :: GrpcClient -> IO (GRpcReply S.SessionsList)
getSessions connection = gRpcCall @'MsgProtoBuf @S.ServerServicer @"Server" @"getSessions" connection S.Empty

getSessionState :: GrpcClient -> S.SessionId -> IO (GRpcReply S.SessionState)
getSessionState = gRpcCall @'MsgProtoBuf @S.ServerServicer @"Server" @"getSessionState"

makeAction :: GrpcClient -> S.ActionRequest -> IO (GRpcReply S.Empty)
makeAction = gRpcCall @'MsgProtoBuf @S.ServerServicer @"Server" @"makeAction"

clickSlot :: GrpcClient -> S.PlayerClickSlotRequest -> IO (GRpcReply S.Empty)
clickSlot = gRpcCall @'MsgProtoBuf @S.ServerServicer @"Server" @"clickSlot"

clickItem :: GrpcClient -> S.PlayerClickItemRequest -> IO (GRpcReply S.Empty)
clickItem = gRpcCall @'MsgProtoBuf @S.ServerServicer @"Server" @"clickItem"

createNewSession :: GrpcClient -> IO (GRpcReply S.SessionId)
createNewSession client = gRpcCall @'MsgProtoBuf @S.ServerServicer @"Server" @"createNewSession" client S.Empty

addNewPlayerToSession :: GrpcClient -> S.SessionId -> IO (GRpcReply S.PlayerId)
addNewPlayerToSession = gRpcCall @'MsgProtoBuf @S.ServerServicer @"Server" @"addNewPlayerToSession"

removePlayerFromSession :: GrpcClient -> S.RemovePlayerRequest -> IO (GRpcReply S.Empty)
removePlayerFromSession = gRpcCall @'MsgProtoBuf @S.ServerServicer @"Server" @"removePlayerFromSession"