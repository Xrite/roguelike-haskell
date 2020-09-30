{-# LANGUAGE ScopedTypeVariables #-}
module Game.Multiplayer.Client where

import Control.Exception
import Game.Environment
import Game.Multiplayer.Server
import qualified Game.Multiplayer.Server as Server
import Game.Unit.Action
import Mu.GRpc.Client.TyApps
import Network.Socket (HostName, PortNumber)
import qualified Networking.Rpc.Client as RPCC
import qualified Networking.Rpc.Schema as S

type ClientHandle = GrpcClient

setupClient :: HostName -> PortNumber -> IO (Maybe ClientHandle)
setupClient ip port = catch getHandle ignore
  where 
    getHandle = do
      r <- RPCC.setupClient ip port
      case r of
        Left _ -> return Nothing
        Right handle -> return $ Just handle
    ignore :: SomeException -> IO (Maybe ClientHandle)
    ignore e = return Nothing

getSessions :: ClientHandle -> IO [SessionId]
getSessions handle = do
  r <- RPCC.getSessions handle
  case r of
    GRpcOk ss -> return $ Server.fromSessionsListSchema ss
    _ -> return []

getSessionState :: ClientHandle -> SessionId -> IO (Maybe EnvMemento)
getSessionState handle sid = do
  let sidSchema = Server.toSessionIdSchema sid
  r <- RPCC.getSessionState handle sidSchema
  case r of
    GRpcOk sessionState -> return $ Server.fromSessionStateSchema sessionState
    _ -> return Nothing

makeAction :: ClientHandle -> SessionId -> PlayerId -> Action -> IO ()
makeAction handle sid pid action = do
  r <- RPCC.makeAction handle request
  return ()
  where
    request = Server.toActionRequestSchema sid pid action

clickSlot :: ClientHandle -> SessionId -> PlayerId -> Int -> IO ()
clickSlot handle sid pid i = do
  r <- RPCC.clickSlot handle request
  return ()
  where
    request = Server.toPlayerClickSlotRequest sid pid i

clickItem :: ClientHandle -> SessionId -> PlayerId -> Int -> IO ()
clickItem handle sid pid i = do
  r <- RPCC.clickItem handle request
  return ()
  where
    request = Server.toPlayerClickItemRequest sid pid i

createNewSession :: ClientHandle -> IO (Maybe SessionId)
createNewSession handle = do
  r <- RPCC.createNewSession handle
  case r of
    GRpcOk sidSchema -> return $ Just (Server.fromSessionIdSchema sidSchema)
    _ -> return Nothing

addNewPlayerToSession :: ClientHandle -> SessionId -> IO (Maybe PlayerId)
addNewPlayerToSession handle sid = do
  let sidSchema = Server.toSessionIdSchema sid
  r <- RPCC.addNewPlayerToSession handle sidSchema
  case r of
    GRpcOk pidSchema -> return $ Just (fromPlayerIdSchema pidSchema)
    _ -> return Nothing

removePlayerFromSession :: ClientHandle -> SessionId -> PlayerId -> IO ()
removePlayerFromSession handle sid pid = do
  RPCC.removePlayerFromSession handle request >> return ()
  where
    request = Server.toRemovePlayerRequest sid pid
