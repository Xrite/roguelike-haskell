{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Multiplayer.Server where

import Brick.BChan
import Control.Concurrent.STM
import Control.Lens hiding ((<|), (|>))
import Control.Monad.State
import Data.Binary (encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Either (fromRight, rights)
import qualified Data.IntMap as IntMap
import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Sequence as Seq
import Data.Sequence ((<|), (|>))
import qualified Data.Set as Set
import qualified Data.Text as T
import Debug.Trace
import Game.Environment
import Game.EnvironmentGeneration
import Game.FileIO.FileIO (getLevelByName)
import Game.FileIO.SaveGame
import Game.GameControl
import qualified Game.GameLevels.Generation.GenerationUtil as GU
import Game.Unit
import Game.Unit.Action
import Game.Unit.Control
import Game.Unit.Inventory
import Game.Unit.Stats
import Game.Unit.TimedUnitOps (empty)
import Game.Unit.Unit
import Mu.Server
import qualified Networking.Rpc.Schema as S
import qualified Networking.Rpc.Server as RPCS
import System.Random

type SessionId = Int

data ServerData
  = ServerData
      { _sessions :: IntMap.IntMap Session,
        _serverRandomGenerator :: StdGen
      }

data Session
  = Session
      { _sessionEnv :: Environment
      }

emptyServerData :: Int -> ServerData
emptyServerData seed =
  ServerData
    { _sessions = IntMap.empty,
      _serverRandomGenerator = mkStdGen seed
    }

newtype ServerEnv a = ServerEnv {unServerEnv :: State ServerData a}
  deriving (Functor, Applicative, Monad, MonadState ServerData)

makeLenses ''ServerData

makeLenses ''Session

makeSession :: Environment -> Session
makeSession env = Session {_sessionEnv = env}

makeSessionId x = x

runServerEnv :: ServerEnv a -> ServerData -> (a, ServerData)
runServerEnv serverEnv = runState (unServerEnv serverEnv)

allSessionIds :: ServerData -> [SessionId]
allSessionIds sd = IntMap.keys $ sd ^. sessions

randomServerEnv :: Random a => ServerEnv a
randomServerEnv = do
  g <- gets _serverRandomGenerator
  let (value, g') = random g
  modify $ set serverRandomGenerator g'
  return value

randomRServerEnv :: Random a => (a, a) -> ServerEnv a
randomRServerEnv range = do
  g <- gets _serverRandomGenerator
  let (value, g') = randomR range g
  modify $ set serverRandomGenerator g'
  return value

-- | Create new session
createNewSession :: ServerEnv SessionId
createNewSession = do
  seed <- randomServerEnv
  let env = randomEnvironmentWithoutUnits seed
  let newSession = makeSession env
  sid <- gets (freeSessionId)
  modify $ over sessions (IntMap.insert sid newSession)
  return sid
  where
    freeSessionId d = let Just idx = find (isFree d) [1 ..] in idx
    isFree d idx = IntMap.notMember idx (d ^. sessions)

-- | Add new player to the session
addNewPlayerToSession :: SessionId -> ServerEnv (Maybe PlayerId)
addNewPlayerToSession sid = do
  mSession <- gets (^? (sessions . ix sid))
  case mSession of
    Nothing -> return Nothing
    Just session -> do
      let env = session ^. sessionEnv
      let positions = getFreePositionsOnLevel env stats level
      i <- randomRServerEnv (0, length positions - 1)
      let selectedPos = positions !! i
      let player = defaultPlayerWithStats selectedPos stats
      let (pid, env') = runGameEnv (addPlayerToEnvironment player) env
      modify $ set (sessions . ix sid . sessionEnv) env'
      return $ Just pid
  where
    stats = defaultStats
    level = 0

-- | Remove player from session if this player is present in the session
removePlayerFromSession :: SessionId -> PlayerId -> ServerEnv ()
removePlayerFromSession sid pid = do
  mSession <- gets (^? (sessions . ix sid))
  case mSession of
    Nothing -> return ()
    Just session -> do
      let env = session ^. sessionEnv
      let (_, env') = runGameEnv (removePlayerFromEnvironment pid) env
      modify' $ set (sessions . ix sid . sessionEnv) env'

-- | Make action
playerMakeAction :: SessionId -> PlayerId -> Action -> ServerEnv ()
playerMakeAction sid pid action = do
  mSession <- gets (^? (sessions . ix sid))
  case mSession of
    Nothing -> return ()
    Just session -> do
      let env = session ^. sessionEnv
      let (_, env') = runGameEnv (makeTurn pid action) env
      modify' $ set (sessions . ix sid . sessionEnv) env'

-- | Click slot
playerClickSlot :: SessionId -> PlayerId -> Int -> ServerEnv ()
playerClickSlot sid pid i = do
  mSession <- gets (^? (sessions . ix sid))
  case mSession of
    Nothing -> return ()
    Just session -> do
      let env = session ^. sessionEnv
      let (_, env') = runGameEnv (doClickSlot pid i) env
      modify' $ set (sessions . ix sid . sessionEnv) env'

-- | Click item
playerClickItem :: SessionId -> PlayerId -> Int -> ServerEnv ()
playerClickItem sid pid i = do
  mSession <- gets (^? (sessions . ix sid))
  case mSession of
    Nothing -> return ()
    Just session -> do
      let env = session ^. sessionEnv
      let (_, env') = runGameEnv (doClickItem pid i) env
      modify' $ set (sessions . ix sid . sessionEnv) env'

type Server = ServerErrorIO

fromSessionIdSchema :: S.SessionId -> SessionId
fromSessionIdSchema (S.SessionId idx) = fromIntegral idx

toSessionIdSchema :: SessionId -> S.SessionId
toSessionIdSchema sid = S.SessionId (fromIntegral sid)

fromActionRequestSchema :: S.ActionRequest -> Maybe (SessionId, PlayerId, Action)
fromActionRequestSchema (S.ActionRequest mSid mPid dx dy) = do
  sid <- fromSessionIdSchema <$> mSid
  pid <- fromPlayerIdSchema <$> mPid
  let action = deltaToAction (fromIntegral dx, fromIntegral dy)
  return (sid, pid, action)

fromPlayerClickSlotRequest :: S.PlayerClickSlotRequest -> Maybe (SessionId, PlayerId, Int)
fromPlayerClickSlotRequest (S.PlayerClickSlotRequest mSid mPid idx) = do
  sid <- fromSessionIdSchema <$> mSid
  pid <- fromPlayerIdSchema <$> mPid
  let i = fromIntegral idx
  return (sid, pid, i)

fromPlayerClickItemRequest :: S.PlayerClickItemRequest -> Maybe (SessionId, PlayerId, Int)
fromPlayerClickItemRequest (S.PlayerClickItemRequest mSid mPid idx) = do
  sid <- fromSessionIdSchema <$> mSid
  pid <- fromPlayerIdSchema <$> mPid
  let i = fromIntegral idx
  return (sid, pid, i)

fromRemovePlayerRequest :: S.RemovePlayerRequest -> Maybe (SessionId, PlayerId)
fromRemovePlayerRequest (S.RemovePlayerRequest mSid mPid) = do
  sid <- fromSessionIdSchema <$> mSid
  pid <- fromPlayerIdSchema <$> mPid
  return (sid, pid)

toSessionStateSchema :: Session -> S.SessionState
toSessionStateSchema s =
  S.SessionState
    { state = Just S.RUNNING,
      content = LBS.toStrict . encode $ getEnvState (s ^. sessionEnv)
    }

notRunningSessionState :: S.SessionState
notRunningSessionState =
  S.SessionState
    { state = Just S.NOT_RUNNING,
      content = BS.empty
    }

toSessionInfoSchema :: SessionId -> S.SessionInfo
toSessionInfoSchema sid =
  S.SessionInfo
    { name = T.pack $ show sid,
      idx = Just $ toSessionIdSchema sid
    }

toSessionsListSchema :: ServerData -> S.SessionsList
toSessionsListSchema sd =
  S.SessionsList
    { sessions = map toSessionInfoSchema $ allSessionIds sd
    }

instance RPCS.RpcServer Server (TVar ServerData) where
  getSessions t = do
    sd <- liftIO $ atomically $ readTVar t
    return $ toSessionsListSchema sd

  getSessionState t sidSchema = do
    let sid = fromSessionIdSchema sidSchema
    liftIO $ atomically $ do
      sd <- readTVar t
      case sd ^? sessions . ix sid of
        Nothing -> return notRunningSessionState
        Just s -> return $ toSessionStateSchema s

  makeAction t request = do
    liftIO $ atomically $ modifyTVar t tryModify
    where
      newData sd = do
        (sid, pid, action) <- fromActionRequestSchema request
        return $ runServerEnv (playerMakeAction sid pid action)
      tryModify sd = case newData sd of
        Nothing -> sd
        Just f -> snd $ f sd

  clickSlot t request = liftIO $ atomically $ modifyTVar t tryModify
    where
      updateServerData sd = do
        (sid, pid, i) <- fromPlayerClickSlotRequest request
        return $ runServerEnv (playerClickSlot sid pid i)
      tryModify sd = case updateServerData sd of
        Nothing -> sd
        Just f -> snd $ f sd

  clickItem t request = liftIO $ atomically $ modifyTVar t tryModify
    where
      updateServerData sd = do
        (sid, pid, i) <- fromPlayerClickItemRequest request
        return $ runServerEnv (playerClickItem sid pid i)
      tryModify sd = case updateServerData sd of
        Nothing -> sd
        Just f -> snd $ f sd

  createNewSession t = do
    liftIO $ atomically $ do
      sd <- readTVar t
      let (sid, sd') = runServerEnv createNewSession sd
      writeTVar t sd'
      return $ toSessionIdSchema sid

  addNewPlayerToSession t sidSchema = do
    let sid = fromSessionIdSchema sidSchema
    liftIO $ atomically $ do
      sd <- readTVar t
      let (mPid, sd') = runServerEnv (addNewPlayerToSession sid) sd
      case mPid of
        Nothing -> error "wrong sessionId"
        Just pid -> do
          writeTVar t sd'
          return $ toPlayerIdSchema pid

  removePlayerFromSession t request = liftIO $ atomically $ modifyTVar t tryModify
    where
      updateServerData sd = do
        (sid, pid) <- fromRemovePlayerRequest request
        return $ runServerEnv (removePlayerFromSession sid pid)
      tryModify sd = case updateServerData sd of
        Nothing -> sd
        Just f -> snd $ f sd
