module Game.Multiplayer.Client where

import Game.Environment
import Game.Unit.Action
import Game.Multiplayer.Server
import qualified Game.Multiplayer.Server as Server
import Mu.GRpc.Client.TyApps 
import qualified Networking.Rpc.Client as RPCC
import qualified Networking.Rpc.Schema as S

type Handle = GrpcClient

getSessionState :: Handle -> SessionId -> IO (Maybe EnvMemento)
getSessionState handle sid = do
  let sidSchema = Server.toSessionIdSchema sid
  r <- RPCC.getSessionState handle sidSchema
  case r of
    GRpcOk sessionState -> return $ Server.fromSessionStateSchema sessionState
    _ -> return Nothing

makeAction :: Handle -> SessionId -> PlayerId -> Action -> IO ()
makeAction handle sid pid action = do
  r <- RPCC.makeAction handle request
  return ()
  where
    request = Server.toActionRequestSchema sid pid action

clickSlot :: Handle -> SessionId -> PlayerId -> Int -> IO ()
clickSlot handle sid pid i = do
  r <- RPCC.clickSlot handle request
  return ()
  where
    request = Server.toPlayerClickSlotRequest sid pid i

clickItem :: Handle -> SessionId -> PlayerId -> Int -> IO ()
clickItem handle sid pid i = do
  r <- RPCC.clickItem handle request
  return ()
  where
    request = Server.toPlayerClickItemRequest sid pid i

createNewSession :: Handle -> IO (Maybe SessionId)
createNewSession handle = do
  r <- RPCC.createNewSession handle
  case r of
    GRpcOk sidSchema -> return $ Just (Server.fromSessionIdSchema sidSchema)
    _ -> return Nothing

addNewPlayerToSession :: Handle -> SessionId -> IO (Maybe PlayerId)
addNewPlayerToSession handle sid = do
  let sidSchema = Server.toSessionIdSchema sid
  r <- RPCC.addNewPlayerToSession handle sidSchema
  case r of
    GRpcOk pidSchema -> return $ Just (fromPlayerIdSchema pidSchema)
    _ -> return Nothing

removePlayerFromSession :: Handle -> SessionId -> PlayerId -> IO ()
removePlayerFromSession handle sid pid = do
  RPCC.removePlayerFromSession handle request >> return ()
  where
    request = Server.toRemovePlayerRequest sid pid
