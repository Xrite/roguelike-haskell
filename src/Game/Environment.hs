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
    renderEnvironment,
    evalAction,
    randomRGameEnv,
    getActiveMobs,
    getAction,
    getActiveUnits,
    getActivePlayer,
    getUnitPosition,
    setStrategy,
    renderEnvironmentVisibleToPlayer,
  )
where

import Control.Lens hiding (levels)
import Control.Monad (void, when)
import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.State
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
import Game.Unit
import Game.Unit.Action as Action
import System.Random

-- | All manipulations with units in environment should use this typegg
data UnitId = MobUnitId Int | PlayerUnitId deriving (Eq)

-- TODO maybe extract units to a different module?
-- TODO comment

-- | Contains description of current game state (e.g. map, mobs)
data Environment
  = Environment
      { _player :: Player,
        -- | Mob's id, mob itself and it's action evaluator
        _mobs :: IntMap.IntMap (Mob, Action -> FailableGameEnv UnitIdError ()),
        _levels :: [GameLevel],
        _currentLevel :: Int,
        _currentUnitTurn :: Int,
        _modifierFactory :: UnitOpFactory,
        _playerEvaluator :: Action -> FailableGameEnv UnitIdError (),
        _randomGenerator :: StdGen,
        _strategy :: TaggedControl -> UnitId -> FailableGameEnv UnitIdError Action
      }

-- | A type for evaluating action on Environment
newtype GameEnv a = GameEnv {unGameEnv :: State Environment a} deriving (Functor, Applicative, Monad, MonadState Environment)

data UnitIdError = InvalidUnitId | NoSuchUnit | UnitCastExcepton deriving (Eq)

type FailableGameEnv err = ExceptT err GameEnv

makeLenses ''Environment

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
makeEnvironment :: Player -> [Mob] -> [GameLevel] -> UnitOpFactory -> Environment
makeEnvironment player mobs levels factory =
  Environment
    { _player = player,
      _mobs = IntMap.fromList $ zip [0 ..] $ zip mobs [defaultEvaluation (MobUnitId i) | i <- [0 ..]],
      _levels = levels,
      _currentLevel = 0,
      _currentUnitTurn = 0,
      _modifierFactory = factory,
      _playerEvaluator = defaultEvaluation PlayerUnitId,
      _randomGenerator = mkStdGen 42,
      _strategy = getControl
    }

-- | This function should remove dead units from environment.
-- It is called after each function that can modify units in the environment. With current implementation of units storage it invalidates 'UnitId'.
-- Item drop (units death modifiers in general) is not yet implemented, so TODO implement death modifiers in filterDead
filterDead :: GameEnv ()
filterDead = do
  env <- get
  modify $ set mobs (newMobs env)
  where
    newMobs env = IntMap.filter (isAlive . (^. _1)) (env ^. mobs)

getActiveUnits :: GameEnv [UnitId]
getActiveUnits = do
  filterDead
  env <- get
  let players = if isAlive (env ^. player) then [PlayerUnitId] else []
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
  if isAlive (env ^. player)
    then return $ PlayerUnitId
    else throwError NoSuchUnit

setStrategy :: TaggedControl -> UnitId -> FailableGameEnv UnitIdError ()
setStrategy tag (MobUnitId i) = do
  modify $ set (mobs . ix i . _1 . controlTag) tag
setStrategy _ _ = throwError UnitCastExcepton

_getMobById :: Int -> FailableGameEnv UnitIdError Mob
_getMobById idx = do
  env <- get
  case env ^? mobs . ix idx . _1 of
    Nothing -> throwError InvalidUnitId
    Just mob -> return mob

setMobById :: Int -> Mob -> Environment -> Environment
setMobById idx mob env = env & mobs . ix idx . _1 .~ mob

{- mobLensById :: Int -> Lens' Environment Mob
mobLensById idx = mobs . ix idx . _1 -}

affectUnit :: UnitId -> UnitOp a -> FailableGameEnv UnitIdError a
affectUnit PlayerUnitId modifier = do
  env <- get
  let (newPlayer, result) = applyUnitOp modifier (env ^. player)
  modify $ set player newPlayer
  lift filterDead
  return result
affectUnit (MobUnitId idx) modifier = do
  mob <- _getMobById idx
  let (newMob, result) = applyUnitOp modifier mob
  modify $ setMobById idx newMob
  lift filterDead
  return result

getUnitPosition :: UnitId -> FailableGameEnv UnitIdError (Int, Int)
getUnitPosition uid = affectUnit uid UnitOp.getPosition

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
    PlayerUnitId -> return . MkPlayer $ (env ^. player)
    MobUnitId i -> do
      mob <- _getMobById i
      return $ MkMob mob

-- | Try to set AnyUnit by Unit id. Return True when success.
_trySetUnit :: UnitId -> AnyUnit -> FailableGameEnv UnitIdError ()
_trySetUnit uid u =
  case (uid, u) of
    (PlayerUnitId, MkPlayer p) -> void (modify (set player p))
    (MobUnitId i, MkMob m) -> void (modify (setMobById i m))
    _ -> throwError UnitCastExcepton

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
    PlayerUnitId -> (env ^. playerEvaluator) a >> return True
    MobUnitId idx -> case env ^? mobs . ix idx . _2 of
      Nothing -> throwError InvalidUnitId
      Just evaluator -> evaluator a >> return True

getCurrentLevel :: GameEnv GameLevel
getCurrentLevel = do
  env <- get
  return $ (env ^. levels) !! (env ^. currentLevel)

playerId :: Environment -> UnitId
playerId _ = PlayerUnitId

getPlayer :: GameEnv UnitId
getPlayer = return PlayerUnitId

getAction :: UnitId -> FailableGameEnv UnitIdError Action
getAction PlayerUnitId = return stayAtPosition
getAction uid@(MobUnitId idx) = do
  env <- get
  tag <- case env ^? mobs . ix idx . _1 . controlTag of
    Nothing -> throwError InvalidUnitId
    Just t -> return t
  (env ^. strategy) tag uid

renderEnvironment :: GameEnv [String]
renderEnvironment = do
  lvl <- getCurrentLevel
  let mp = lvl ^. lvlMap
  let ((xFrom, yFrom), (xTo, yTo)) = getMapSize mp
  let terrain = [[renderCell $ getCell (x, y) mp | x <- [xFrom .. xTo]] | y <- [yFrom .. yTo]]
  units <- getActiveUnits
  positions <- rights <$> traverse (runExceptT . (`affectUnit` UnitOp.getPosition)) units
  portraits <- rights <$> traverse (runExceptT . (`affectUnit` UnitOp.getPortrait)) units
  return $ foldl (\m ((x, y), p) -> m & ix y . ix x .~ p) terrain (zip positions portraits)

renderEnvironmentWithPositions :: [(Int, Int)] -> GameEnv [String]
renderEnvironmentWithPositions positionsToRender = do
  lvl <- getCurrentLevel
  let mp = lvl ^. lvlMap
  let ((xFrom, yFrom), (xTo, yTo)) = getMapSize mp
  let terrain = [[if (x, y) `elem` positionsToRender then renderCell $ getCell (x, y) mp else ' ' | x <- [xFrom .. xTo]] | y <- [yFrom .. yTo]]
  units <- getActiveUnits
  positions <- rights <$> traverse (runExceptT . (`affectUnit` UnitOp.getPosition)) units
  portraits <- rights <$> traverse (runExceptT . (`affectUnit` UnitOp.getPortrait)) units
  return $ foldl (\m ((x, y), p) -> if (x, y) `elem` positionsToRender then m & ix y . ix x .~ p else m) terrain (zip positions portraits)

renderEnvironmentVisibleToPlayer :: FailableGameEnv UnitIdError [String]
renderEnvironmentVisibleToPlayer = do
  lvl <- lift getCurrentLevel
  uid <- getActivePlayer
  playerPos <- getUnitPosition uid
  maybeStats <- affectUnit uid UnitOp.getStats
  let positions = visiblePositions (lvl ^. lvlMap) (getVisibility maybeStats) playerPos
  lift $ renderEnvironmentWithPositions positions

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
      let available = allVisible \\ (allUnitPositions \\ [unitPos])
      randomIndex <- lift $ randomRGameEnv (0, length available - 1)
      let newDestination = available !! randomIndex
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
