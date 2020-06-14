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
