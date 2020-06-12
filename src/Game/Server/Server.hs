{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Server.Server where

import Brick.BChan
import Control.Lens hiding ((<|), (|>))
import Control.Monad.State
import Data.Either (fromRight, rights)
import qualified Data.IntMap as IntMap
import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Sequence as Seq
import Data.Sequence ((<|), (|>))
import qualified Data.Set as Set
import Debug.Trace
import Game.Environment
import Game.EnvironmentGeneration
import Game.FileIO.FileIO (getLevelByName)
import Game.FileIO.SaveGame
import qualified Game.GameLevels.Generation.GenerationUtil as GU
import Game.Transaction (Transaction)
import qualified Game.Transaction as Transaction
import Game.Unit
import Game.Unit.Action
import Game.Unit.Control
import Game.Unit.Inventory
import Game.Unit.Stats
import Game.Unit.TimedUnitOps (empty)
import Game.Unit.Unit
import System.Random (getStdRandom, mkStdGen, random, randomRIO)

type SessionId = Int

data ServerData
  = ServerData
      { _sessions :: IntMap.IntMap Session
      }

data Session
  = Session
      { _sessionEnv :: Environment
      }

emptyServer :: ServerData
emptyServer =
  ServerData
    { _sessions = IntMap.empty
    }

newtype Server a = Server {runServer :: StateT ServerData IO a}
  deriving (Functor, Applicative, Monad, MonadState ServerData, MonadIO)


makeSession env = Session {_sessionEnv = env}

makeLenses ''ServerData
makeLenses ''Session

-- | Create new session
createNewSession :: Server SessionId
createNewSession = do
  seed <- liftIO $ getStdRandom random
  let env = randomEnvironmentWithoutUnits seed
  let newSession = makeSession env
  sid <- gets (freeSessionId)
  modify $ over sessions (IntMap.insert sid newSession)
  return sid
  where
    freeSessionId d = let Just idx = find (isFree d) [1 ..] in idx
    isFree d idx = IntMap.notMember idx (d ^. sessions)

-- | Add new player to the session
addNewPlayerToSession :: SessionId -> Server (Maybe PlayerId)
addNewPlayerToSession sid = do
  mSession <- gets (^? (sessions . ix sid))
  case mSession of
    Nothing -> return Nothing
    Just session -> do
      let env = session ^. sessionEnv
      let positions = getFreePositionsOnLevel env stats level
      i <- liftIO $ randomRIO (0, length positions - 1)
      let selectedPos = positions !! i
      let player = defaultPlayerWithStats selectedPos stats
      let (pid, env') = runGameEnv (addPlayerToEnvironment player) env
      modify $ set (sessions . ix sid . sessionEnv) env'
      return $ Just pid
  where
    stats = defaultStats
    level = 0


-- | Remove player from session if this player is present in the session
removePlayerFromSession :: SessionId -> PlayerId -> Server ()
removePlayerFromSession sid pid = do
  mSession <- gets (^? (sessions . ix sid))
  case mSession of
    Nothing -> return ()
    Just session -> do
      let env = session ^. sessionEnv
      let (_, env') = runGameEnv (removePlayerFromEnvironment pid) env
      modify' $ set (sessions . ix sid . sessionEnv) env'

-- | Join the session. Returns id of created player.
joinSession :: SessionId -> Server PlayerId
joinSession = undefined

-- | Make action
playerMakeAction :: SessionId -> PlayerId -> Action -> Server ()
playerMakeAction sid pid action = do
  mSession <- gets (^? (sessions . ix sid))
  case mSession of
    Nothing -> return ()
    Just session -> do
      let env = session ^. sessionEnv
      let trans = Transaction.unitAction pid action
      let (_, env') = runGameEnv (Transaction.applyTransaction trans) env
      modify' $ set (sessions . ix sid . sessionEnv) env'

-- | Click slot
playerClickSlot :: SessionId -> PlayerId -> Int -> Server ()
playerClickSlot sid pid i = do
  mSession <- gets (^? (sessions . ix sid))
  case mSession of
    Nothing -> return ()
    Just session -> do
      let env = session ^. sessionEnv
      let trans = Transaction.clickSlot pid i
      let (_, env') = runGameEnv (Transaction.applyTransaction trans) env
      modify' $ set (sessions . ix sid . sessionEnv) env'

-- | Click item
playerClickItem :: SessionId -> PlayerId -> Int -> Server ()
playerClickItem sid pid i = do
  mSession <- gets (^? (sessions . ix sid))
  case mSession of
    Nothing -> return ()
    Just session -> do
      let env = session ^. sessionEnv
      let trans = Transaction.clickItem pid i
      let (_, env') = runGameEnv (Transaction.applyTransaction trans) env
      modify' $ set (sessions . ix sid . sessionEnv) env'
