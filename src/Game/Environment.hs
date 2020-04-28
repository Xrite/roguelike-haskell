{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Environment
  ( Environment,
    UnitId,
    GameEnv,
    FailableGameEnv,
    UnitIdError (..),
    makeEnvironment,
    getCurrentLevel,
    unitByCoord,
    envAttack,
    playerId,
    getPlayer,
    affectUnit,
    queryUnitWithModifiers,
    moveUnit,
    runGameEnv,
    evalAction,
    randomRGameEnv,
    getActiveMobs,
    getAction,
    getActiveUnits,
    getActivePlayer,
    getUnitPosition,
    getUnitPortrait,
    setStrategy,
    getLevellingStats,
    getUnitStats,
    getVisibleToPlayer,
    getSeenByPlayer,
    getCells,
  )
where

import Control.Lens hiding (levels)
import Control.Monad (void, when)
import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.State
import Data.Array
import qualified Data.Set as Set
import Data.Either (rights)
import qualified Data.IntMap as IntMap
import Data.List
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing, listToMaybe)
import Game.GameLevels.GameLevel
import Game.GameLevels.MapCell
import Game.GameLevels.MapCellType
import Game.GameLevels.PathFinding
import Game.GameLevels.Visibility
import Game.Modifiers.UnitOp as UnitOp
import Game.Modifiers.UnitOpFactory (UnitOpFactory)
import Game.Modifiers.DefaultUnitOpFactory (defaultUnitOpFactory)
import Game.Unit
import Game.Unit.Action as Action
import System.Random
import Safe (atDef)

-- | All manipulations with units in environment should use this type
data UnitId = MobUnitId Int | PlayerUnitId deriving (Eq)

-- TODO maybe extract units to a different module?
-- TODO comment

-- | Contains description of current game state (e.g. map, mobs)
data Environment
  = Environment
      { _player :: WithEvaluator Player,
        -- | Mob's id, mob itself and it's action evaluator
        _mobs :: IntMap.IntMap (WithEvaluator Mob), -- evaluator (Action -> FailableGameEnv UnitIdError ()) removed
        _levels :: [GameLevel],
        _currentLevel :: Int,
        _currentUnitTurn :: Int,
        _modifierFactory :: UnitOpFactory,
        _randomGenerator :: StdGen,
--        _strategy :: TaggedControl -> UnitId -> FailableGameEnv UnitIdError Action,  --^ Replaced with a constant Getter (see strategy)
        _seenByPlayer :: [Set.Set (Int, Int)]
      }

data WithEvaluator a
  = WithEvaluator
      { _object :: a,
        _objectId :: UnitId
      }

makeLenses ''WithEvaluator

-- | A type for evaluating action on Environment
newtype GameEnv a = GameEnv {unGameEnv :: State Environment a} deriving (Functor, Applicative, Monad, MonadState Environment)

data UnitIdError = InvalidUnitId | NoSuchUnit | UnitCastException deriving (Eq)

type FailableGameEnv err = ExceptT err GameEnv

makeLenses ''Environment

strategy :: Getter Environment (TaggedControl -> UnitId -> FailableGameEnv UnitIdError Action)
strategy = to $ const getControl

evaluator :: Getter (WithEvaluator a) (Action -> FailableGameEnv UnitIdError ())
evaluator = to (defaultEvaluation . _objectId)

instance MonadFail GameEnv where
  fail = error

instance Show Environment where
  show _ = "Environment"

randomRGameEnv :: Random a => (a, a) -> GameEnv a
randomRGameEnv range = do
  g <- gets _randomGenerator
  let (value, g') = randomR range g
  randomGenerator .= g'
  return value

runGameEnv :: GameEnv a -> Environment -> (a, Environment)
runGameEnv gameEnv = runState (unGameEnv gameEnv)

-- | Constructs a new 'Environment'.
makeEnvironment :: Player -> [Mob] -> [GameLevel] -> Environment
makeEnvironment player mobs levels =
  Environment
    { _player = WithEvaluator player PlayerUnitId
    , _mobs = IntMap.fromList [(i, WithEvaluator m $ MobUnitId i) | (i, m) <- zip [0 ..] mobs]
    , _levels = levels
    , _currentLevel = 0
    , _currentUnitTurn = 0
    , _modifierFactory = defaultUnitOpFactory
    , _randomGenerator = mkStdGen 42
--      _strategy = getControl,
    , _seenByPlayer = [Set.empty | _ <- [0 ..]]
    }

-- | This function should remove dead units from environment.
-- It is called after each function that can modify units in the environment. With current implementation of units storage it invalidates 'UnitId'.
-- Item drop (units death modifiers in general) is not yet implemented, so TODO implement death modifiers in filterDead
filterDead :: GameEnv ()
filterDead = do
  env <- get
  modify $ set mobs (newMobs env)
  where
    newMobs env = IntMap.filter (isAlive . (^. object)) (env ^. mobs)

getActiveUnits :: GameEnv [UnitId]
getActiveUnits = do
  filterDead
  env <- get
  let players = if isAlive (env ^. player . object) then [PlayerUnitId] else []
  activeMobs <- getActiveMobs
  return $ players ++ activeMobs

getActiveMobs :: GameEnv [UnitId]
getActiveMobs = do
  filterDead
  env <- get
  let activeMobs = map MobUnitId $ IntMap.keys (env ^. mobs)
  return activeMobs

getActivePlayer :: FailableGameEnv UnitIdError UnitId
getActivePlayer = do
  env <- get
  if isAlive (env ^. player . object)
    then return PlayerUnitId
    else throwError NoSuchUnit

getPlayer :: GameEnv UnitId
getPlayer = return PlayerUnitId

setStrategy :: TaggedControl -> UnitId -> FailableGameEnv UnitIdError ()
setStrategy tag (MobUnitId i) = modify $ set (mobs . ix i . object . controlTag) tag
setStrategy _ _ = throwError UnitCastException

_getMobById :: Int -> FailableGameEnv UnitIdError Mob
_getMobById idx = do
  env <- get
  case env ^? mobs . ix idx . object of
    Nothing -> throwError InvalidUnitId
    Just mob -> return mob

setMobById :: Int -> Mob -> Environment -> Environment
setMobById idx mob env = env & mobs . ix idx . object .~ mob

getLevellingStats :: UnitId -> FailableGameEnv UnitIdError LevellingStats
getLevellingStats PlayerUnitId = gets (view (player . object . levelling))
getLevellingStats _ = throwError UnitCastException

{- mobLensById :: Int -> Lens' Environment Mob
mobLensById idx = mobs . ix idx . _1 -}

affectUnit :: UnitId -> UnitOp a -> FailableGameEnv UnitIdError a
affectUnit PlayerUnitId modifier = do
  lift updateSeenByPlayer
  env <- get
  let (newPlayer, result) = applyUnitOp modifier (env ^. player . object)
  player . object .= newPlayer
  lift filterDead
  lift updateSeenByPlayer
  return result
affectUnit (MobUnitId idx) modifier = do
  mob <- _getMobById idx
  let (newMob, result) = applyUnitOp modifier mob
  modify $ setMobById idx newMob
  lift filterDead
  return result

getUnitPosition :: UnitId -> FailableGameEnv UnitIdError (Int, Int)
getUnitPosition uid = affectUnit uid UnitOp.getPosition

getUnitPortrait :: UnitId -> FailableGameEnv UnitIdError Char
getUnitPortrait uid = affectUnit uid UnitOp.getPortrait 

getUnitStats :: UnitId -> FailableGameEnv UnitIdError Stats
getUnitStats uid = do
  maybeStats <- affectUnit uid UnitOp.getStats
  case maybeStats of
    Nothing -> throwError InvalidUnitId
    Just stats -> return stats

queryUnitWithModifiers :: UnitId -> UnitOp a -> FailableGameEnv UnitIdError a
queryUnitWithModifiers idx modifier = do
  unit <- _accessUnit idx
  fact <- use modifierFactory
  let modifiedUnit = unitWithModifiers fact unit
  let (_, res) = applyUnitOp modifier modifiedUnit
  return res

-- | Get unit at position
unitByCoord :: (Int, Int) -> GameEnv (Maybe UnitId)
unitByCoord coord = do
  units <- getActiveUnits
  filtered <- filterM (\u -> (== Right coord) <$> runExceptT (affectUnit u UnitOp.getPosition)) units
  return $ listToMaybe filtered

moveUnit :: UnitId -> (Int, Int) -> FailableGameEnv UnitIdError Bool
moveUnit u pos = do
  isPassable <- checkPassable u pos
  if isPassable
    then affectUnit u (UnitOp.setCoord pos) >> return True
    else return False

checkPassable :: UnitId -> (Int, Int) -> FailableGameEnv UnitIdError Bool
checkPassable uid pos = do
  lvl <- lift getCurrentLevel
  unitPos <- affectUnit uid UnitOp.getPosition
  let maybeCell = maybeGetCellAt pos lvl
  maybeStats <- affectUnit uid UnitOp.getStats
  isFree <- isNothing <$> lift (unitByCoord pos)
  if pos == unitPos || isJust (checkAll maybeCell maybeStats isFree)
    then return True
    else return False
  where
    checkAll maybeCell maybeStats isFree = do
      cell <- maybeCell
      stats <- maybeStats
      guard isFree
      guard $ (cell ^. cellType . passable) stats
      return ()

{- unitByCoord :: (Int, Int) -> Environment -> Maybe Unit.AnyUnit
unitByCoord coord env = find ((== coord) . _position . Unit.asUnitData) $ _units env -}

-- | Get AnyUnit from UnitId. Returns Nothing if UnitId is invalid.
_accessUnit :: UnitId -> FailableGameEnv UnitIdError AnyUnit
_accessUnit uid = do
  env <- get
  case uid of
    PlayerUnitId -> return . MkPlayer $ (env ^. player . object)
    MobUnitId i -> do
      mob <- _getMobById i
      return $ MkMob mob

-- | Try to set AnyUnit by Unit id. Return True when success.
_trySetUnit :: UnitId -> AnyUnit -> FailableGameEnv UnitIdError ()
_trySetUnit uid u =
  case (uid, u) of
    (PlayerUnitId, MkPlayer p) -> player . object .= p
    (MobUnitId i, MkMob m) -> modify (setMobById i m)
    _ -> throwError UnitCastException

-- | Perform and attack between two units
envAttack ::
  -- | Attacker
  UnitId ->
  -- | Attacked
  UnitId ->
  FailableGameEnv UnitIdError ()
envAttack attackerId attackedId = do
  env <- get
  let fact = env ^. modifierFactory
  attacker <- _accessUnit attackerId
  attacked <- _accessUnit attackedId
  let (attacker', attacked') = attack fact attacker attacked
  _trySetUnit attackerId attacker' -- should always be true
  _trySetUnit attackedId attacked'
  return ()

evalAction :: UnitId -> Action -> FailableGameEnv UnitIdError Bool
evalAction u a = do
  env <- get
  case u of
    PlayerUnitId -> (env ^. player . evaluator) a >> return True
    MobUnitId idx -> case env ^? mobs . ix idx . evaluator of
      Nothing -> throwError InvalidUnitId
      Just unitEvaluator -> unitEvaluator a >> return True

getCurrentLevel :: GameEnv GameLevel
getCurrentLevel = do
  env <- get
  return $ atDef (error "currentLevel index out of bounds") (env ^. levels) (env ^. currentLevel)

playerId :: Environment -> UnitId
playerId _ = PlayerUnitId

getAction :: UnitId -> FailableGameEnv UnitIdError Action
getAction PlayerUnitId = return stayAtPosition
getAction uid@(MobUnitId idx) = do
  env <- get
  tag <- case env ^? mobs . ix idx . object . controlTag of
    Nothing -> throwError InvalidUnitId
    Just t -> return t
  (env ^. strategy) tag uid

getVisibleToPlayer :: GameEnv [(Int, Int)]
getVisibleToPlayer = do
  env <- get
  lvl <- getCurrentLevel
  let visibility = getVisibility (Just $ env ^. player . object . playerUnit . stats)
  let playerPos = env ^. player . object . playerUnit . position
  return $ visiblePositions (lvl ^. lvlMap) visibility playerPos

getSeenByPlayer :: GameEnv [(Int, Int)]
getSeenByPlayer = do env <- get; return . Set.toList $ atDef (error "currentLevel index out of bounds") (env ^. seenByPlayer) (env ^. currentLevel)

getCells :: GameEnv (Array (Int, Int) Char)
getCells = do
  lvl <- getCurrentLevel
  return $ view (cellType . cellRender) <$> lvl ^. lvlMap . cells

updateSeenByPlayer :: GameEnv ()
updateSeenByPlayer = do
  lvlNum <- gets $ view currentLevel
  visible <- Set.fromList <$> getVisibleToPlayer
  modify $ over (seenByPlayer . ix lvlNum) (Set.union visible)

--------------------------------------------------------------------
-- Control
--------------------------------------------------------------------

getControl :: TaggedControl -> UnitId -> FailableGameEnv UnitIdError Action
getControl Aggressive = aggressiveControl
getControl (Passive dest) = passiveControl dest
getControl Avoiding = avoidingControl
getControl DoNothing = const $ return stayAtPosition

-- | Tries to reach and attack a player whenever player is visible to this unit
aggressiveControl :: UnitId -> FailableGameEnv UnitIdError Action
aggressiveControl uid = do
  lvl <- lift getCurrentLevel
  playerPos <- getActivePlayer >>= getUnitPosition
  unitPos <- getUnitPosition uid
  maybeStats <- affectUnit uid UnitOp.getStats
  allUnits <- lift getActiveUnits >>= traverse (`affectUnit` UnitOp.getPosition)
  let path = findPath (getPassability maybeStats) (lvl ^. lvlMap) unitPos playerPos (allUnits \\ [playerPos, unitPos])
  if canSee (lvl ^. lvlMap) (getVisibility maybeStats) unitPos playerPos
    then case path of
      Nothing -> return stayAtPosition
      Just (_ : (x, y) : _) -> return $ deltaToAction (x - fst unitPos, y - snd unitPos)
      Just _ -> return stayAtPosition
    else return stayAtPosition

passiveControl :: (Int, Int) -> UnitId -> FailableGameEnv UnitIdError Action
passiveControl dest uid = do
  unitPos <- getUnitPosition uid
  if unitPos == dest
    then generateNewDestination unitPos
    else do
      lvl <- lift getCurrentLevel
      maybeStats <- affectUnit uid UnitOp.getStats
      allUnits <- lift getActiveUnits >>= traverse (`affectUnit` UnitOp.getPosition)
      let path = findPath (getPassability maybeStats) (lvl ^. lvlMap) unitPos dest (allUnits \\ [unitPos])
      case path of
        Nothing -> generateNewDestination unitPos
        Just (_ : (x, y) : _) -> return $ deltaToAction (x - fst unitPos, y - snd unitPos)
        Just _ -> return stayAtPosition
  where
    generateNewDestination unitPos = do
      lvl <- lift getCurrentLevel
      maybeStats <- affectUnit uid UnitOp.getStats
      allUnitPositions <- lift getActiveUnits >>= traverse (`affectUnit` UnitOp.getPosition)
      let allVisible = visiblePositions (lvl ^. lvlMap) (getVisibility maybeStats) unitPos
      let available = unitPos : (allVisible \\ allUnitPositions)
      randomIndex <- lift $ randomRGameEnv (0, length available - 1)
      let newDestination = atDef (error $ "wrong available position index: " ++ show randomIndex ++ " out of " ++ show (length available)) available randomIndex
      setStrategy (Passive newDestination) uid
      return stayAtPosition

avoidingControl :: UnitId -> FailableGameEnv UnitIdError Action
avoidingControl uid = do
  lvl <- lift getCurrentLevel
  playerPos <- getActivePlayer >>= getUnitPosition
  unitPos <- getUnitPosition uid
  maybeStats <- affectUnit uid UnitOp.getStats
  allUnitPositions <- lift getActiveUnits >>= traverse (`affectUnit` UnitOp.getPosition)
  let possible = getFurtherFrom (lvl ^. lvlMap) (getPassability maybeStats) playerPos unitPos allUnitPositions
  case possible of
    [] -> return stayAtPosition
    xs -> do
      randomIndex <- lift $ randomRGameEnv (0, length xs - 1)
      let (x, y) = xs !! randomIndex
      return $ deltaToAction (x - fst unitPos, y - snd unitPos)
  where
    distance2 (x1, y1) (x2, y2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2

getPassability maybeStats cell = case maybeStats of
  Nothing -> False
  Just stats -> (cell ^. cellType . passable) stats

getVisibility maybeStats cell = case maybeStats of
  Nothing -> False
  Just stats -> (cell ^. cellType . transparent) stats

type ActionEvaluator = UnitId -> Action -> FailableGameEnv UnitIdError ()

basicEvaluation :: ActionEvaluator
basicEvaluation u (Move xDir yDir) = do
  position <- affectUnit u UnitOp.getPosition
  let newPosition = Action.changeCoord xDir yDir position
  unitAtPos <- lift $ unitByCoord newPosition
  case unitAtPos of
    Nothing -> void $ moveUnit u newPosition
    Just other -> when (other /= u) $ envAttack u other

defaultEvaluation :: ActionEvaluator
defaultEvaluation = confuseAwareDecorator basicEvaluation

confuseAwareDecorator :: ActionEvaluator -> ActionEvaluator
confuseAwareDecorator eval u dir = do
  isConfused <- queryUnitWithModifiers u UnitOp.getConfusion
  let eval' =
        if isConfused
          then confusedDecorator eval
          else eval
  eval' u dir

-- | Changes ActionEvaluator so that performed action is changed randomly.
--  Move direction is changed to an adjacent with probability 0.5.
confusedDecorator :: ActionEvaluator -> ActionEvaluator
confusedDecorator eval u dir = do
  rand <- lift $ randomRGameEnv (0.0, 1.0)
  let dir'
        | rand > changeDirectionProbability = dir
        | rand > (changeDirectionProbability / 2) = nextDirection dir
        | otherwise = prevDirection dir
  eval u dir'
  where
    changeDirectionProbability = 0.25 :: Double
    directionsCycle =
      [ Move Positive Positive,
        Move Positive Zero,
        Move Positive Negative,
        Move Zero Negative,
        Move Negative Negative,
        Move Negative Zero,
        Move Negative Positive,
        Move Zero Positive,
        Move Positive Positive
      ]
    nextDirection (Move Zero Zero) = Move Zero Zero
    nextDirection d = head $ drops (== d) directionsCycle
    prevDirection = foldl1 (.) $ replicate 7 nextDirection
    drops f (x : xs) = if f x then xs else drops f xs
    drops _ [] = error "element no found"

{--------------------------------------------------------------------
  Save / Load
--------------------------------------------------------------------}
