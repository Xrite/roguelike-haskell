{-# LANGUAGE DeriveGeneric #-}
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
    queryUnitWithModifiers,
    runGameEnv,
    makeTurn,
    randomRGameEnv,
    getActiveMobs,
    getAction,
    getActiveUnits,
    getVisibleToUnit,
    getSeenByPlayer,
    EnvMemento,
    getEnvState,
    loadEnvironmentState,
  )
where

import Control.Lens hiding (levels)
import Control.Monad (void, when)
import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.State
import Data.Array
import Data.Either (rights)
import qualified Data.IntMap as IntMap
import Data.List
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing, listToMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Game.GameLevels.GameLevel
import Game.GameLevels.MapCell
import Game.GameLevels.MapCellType
import Game.GameLevels.PathFinding
import Game.GameLevels.Visibility
import Game.Modifiers.DefaultUnitOpFactory (defaultUnitOpFactory)
import Game.Modifiers.UnitOp as UnitOp
import Game.Modifiers.UnitOpFactory (UnitOpFactory)
import Game.Unit
import Game.Unit.Action as Action
import Safe (atDef)
import System.Random

-- | All manipulations with units in environment should use this type
data UnitId
  = MobUnitId MobId
  | PlayerUnitId PlayerId
  deriving (Eq, Generic, Show)

newtype MobId = MobId Int
  deriving (Eq, Show, Generic)

newtype PlayerId = PlayerId Int
  deriving (Eq, Show, Generic)

-- TODO maybe extract units to a different module?
-- TODO comment

-- | Position in the environment
data Position
  = Position
      { _posLevel :: Int,
        _posX :: Int,
        _posY :: Int
      }
      deriving (Eq, Ord)

makeLenses ''Position


type EPlayer = Player Position

type EMob = Mob Position

type EUnit = Unit Position

type EUnitOp a = UnitOp Position a

type EUnitOpFactory = UnitOpFactory Position

instance Eq StdGen where
  g1 == g2 = show g1 == show g2

-- | All positions in the environment have been seen by a player
newtype SeenByPlayer = SeenByPlayer (Seq.Seq (Set.Set (Int, Int)))


-- | Contains description of current game state (e.g. map, mobs)
data Environment
  = Environment
      { -- | Players with their evaluators
        _players :: IntMap.IntMap (WithEvaluator EPlayer),
        -- | Mobs with their evaluators
        _mobs :: IntMap.IntMap (WithEvaluator EMob),
        -- | Levels on this environment
        _levels :: Seq.Seq GameLevel,
        _currentUnitTurn :: Int,
        -- | An order in which units make turns
        _unitQueue :: Seq.Seq UnitId,
        _modifierFactory :: EUnitOpFactory,
        _randomGenerator :: StdGen,
        _strategy :: TaggedControl -> MobId -> FailableGameEnv UnitIdError Action,
        _seenByPlayer :: IntMap.IntMap SeenByPlayer
      }
  deriving (Generic)


-- | A box for a unit with it's evaluator.
data WithEvaluator a
  = WithEvaluator
      { _object :: a,
        _evaluator :: Action -> FailableGameEnv UnitIdError ()
      }
  deriving (Generic)

-- | A type for evaluating action on Environment
newtype GameEnv a = GameEnv {unGameEnv :: State Environment a} deriving (Functor, Applicative, Monad, MonadState Environment)

-- | An error that could occur when evaluating actions on environment
data UnitIdError
  = InvalidUnitId
  | NoSuchUnit
  | UnitCastException
  | WrongUnitTurn
  deriving (Eq)

type FailableGameEnv err = ExceptT err GameEnv

makeLenses ''Environment

makeLenses ''WithEvaluator

instance MonadFail GameEnv where
  fail = error

instance Show Environment where
  show _ = "Environment"

-- | Make a position in environment.
--  Returns Nothing if no such position exists in this environment
makePosition ::
  Environment ->
  -- | level
  Int ->
  -- | x coordinate
  Int ->
  -- | y coordinate
  Int ->
  Maybe Position
makePosition env l x y = do
  mp <- env ^? levels . ix l . lvlMap
  guard $ inBounds mp (x, y)
  return
    Position
      { _posLevel = l,
        _posX = x,
        _posY = y
      }

-- | (x, y) coordinates of the position
positionXY :: Position -> (Int, Int)
positionXY pos = (pos ^. posX, pos ^. posY) 

levelsCount :: Environment -> Int
levelsCount env = env ^. levels & Seq.length

emptySeenByPlayer :: SeenByPlayer
emptySeenByPlayer = SeenByPlayer Seq.empty

makeSeenByPlayer :: [Position] -> SeenByPlayer
makeSeenByPlayer positions = foldl' addToSeenByPlayer emptySeenByPlayer positions

addToSeenByPlayer :: SeenByPlayer -> Position -> SeenByPlayer
addToSeenByPlayer (SeenByPlayer s) p = SeenByPlayer $ Seq.adjust (Set.insert (p ^. posX, p ^. posY)) (p ^. posLevel) s

bulkAddToSeenByPlayer :: Foldable t =>
                           t Position -> SeenByPlayer -> SeenByPlayer
bulkAddToSeenByPlayer ps s = foldl' addToSeenByPlayer s ps

randomRGameEnv :: Random a => (a, a) -> GameEnv a
randomRGameEnv range = do
  g <- gets _randomGenerator
  let (value, g') = randomR range g
  randomGenerator .= g'
  return value

runGameEnv :: GameEnv a -> Environment -> (a, Environment)
runGameEnv gameEnv = runState (unGameEnv gameEnv)

makeEnvironmentInternal :: [(Int, EPlayer)] -> [(Int, EMob)] -> [GameLevel] -> StdGen -> [(Int, SeenByPlayer)] -> Environment
makeEnvironmentInternal indexedPlayers indexedMobs levels gen indexedSeen =
  Environment
    { _players = IntMap.fromList [(i, WithEvaluator p $ defaultEvaluation $ PlayerUnitId (PlayerId i)) | (i, p) <- indexedPlayers],
      _mobs = IntMap.fromList [(i, WithEvaluator m $ defaultEvaluation $ MobUnitId (MobId i)) | (i, m) <- indexedMobs],
      _levels = Seq.fromList levels,
      _currentUnitTurn = 0,
      _unitQueue = undefined,
      _modifierFactory = defaultUnitOpFactory,
      _randomGenerator = gen,
      _strategy = getControl,
      _seenByPlayer = IntMap.fromList indexedSeen
    }

-- | Constructs a new 'Environment'.
makeEnvironment :: [EPlayer] -> [EMob] -> [GameLevel] -> Environment
makeEnvironment players mobs levels =
  makeEnvironmentInternal
    indexedPlayers
    indexedMobs
    levels
    (mkStdGen 42)
    ([(i, makeSeenByPlayer (allPlayerSeenPositions player))  | (i, player) <- indexedPlayers])
  where
    visibility player = getVisibility (Just $ player ^. playerUnitData . stats)
    getVisible player lvl = visiblePositions (lvl ^. lvlMap) (visibility player) (playerXY player)
    allPlayerSeenPositions player = [Position l x y  | (l, lvl) <- zip [0..] levels, (x, y) <- getVisible player lvl]
    indexedPlayers = zip [1..] players
    indexedMobs = zip [1..] mobs
    playerXY player = let p = player ^. playerUnitData . position in (p ^. posX, p ^. posY)


-- | This function should remove dead units from environment.
-- It is called after each function that can modify units in the environment. With current implementation of units storage it invalidates 'UnitId'.
-- Item drop (units death modifiers in general) is not yet implemented, so TODO implement death modifiers in filterDead
{- removeDeadMobs :: GameEnv ()
filterDead = do
  env <- get
  modify $ set mobs (newMobs env)
  where
    newMobs env = IntMap.filter (isAlive . (^. object)) (env ^. mobs) -}

-- | Get all active (still alive) units from the environment
getActiveUnits :: Environment -> [UnitId]
getActiveUnits env = players ++ mobs
  where
    players = map PlayerUnitId $ getActivePlayers env
    mobs = map MobUnitId $ getActiveMobs env

-- | Get all active (still alive) players from the environment
getActivePlayers :: Environment -> [PlayerId]
getActivePlayers env = activePlayers
  where
    activePlayers = map PlayerId $ IntMap.keys (env ^. players)

-- | Get all active (still alive) players from the environment
getActiveMobs :: Environment -> [MobId]
getActiveMobs env = activeMobs
  where
    activeMobs = map MobId $ IntMap.keys (env ^. mobs)

{- getActivePlayer :: FailableGameEnv UnitIdError UnitId
getActivePlayer = do
  env <- get
  if isAlive (env ^. player . object)
    then return PlayerUnitId
    else throwError NoSuchUnit -}

{- isPlayerAlive :: Environment -> Bool
isPlayerAlive env = isAlive (env ^. player . object) -}

-- | Checks whether a unit is still alive. Returns False if the unit is not present in the environment.
isUnitAlive :: UnitId -> Environment -> Bool
isUnitAlive uid env = case uid of
  PlayerUnitId (PlayerId i) -> IntMap.member i (env ^. players)
  MobUnitId (MobId i) -> IntMap.member i (env ^. mobs)

{- getPlayer :: GameEnv UnitId
getPlayer = return PlayerUnitId -}

{- getPlayerInventory :: GameEnv Inventory
getPlayerInventory = gets $ view (player . object . playerUnit . inventory) -}

-- | Sets an inventory to the unit. Can fail if unit is not present in the environment.
setUnitInventory :: UnitId -> Inventory -> FailableGameEnv UnitIdError ()
setUnitInventory uid inv = case uid of
  PlayerUnitId (PlayerId i) -> modify $ set (players . ix i . object . playerUnitData . inventory) inv
  MobUnitId (MobId i) -> modify $ set (mobs . ix i . object . mobUnitData . inventory) inv

unitEquipItem :: UnitId -> Int -> FailableGameEnv UnitIdError ()
unitEquipItem uid i = do
  unit <- gets (getUnitByUnitId uid) >>= liftEither
  let inv = getUnitData unit ^. inventory
  case tryEquipItem i inv of
    (Right inv') -> setUnitInventory uid inv'
    (Left _) -> return ()

unitFreeHeadSlot :: UnitId -> GameEnv ()
unitFreeHeadSlot uid = case uid of
  PlayerUnitId (PlayerId i) -> (players . ix i . object . playerUnitData . inventory) %= freeHeadSlot
  MobUnitId (MobId i) -> (mobs . ix i . object . mobUnitData . inventory) %= freeHeadSlot

unitFreeChestSlot :: UnitId -> GameEnv ()
unitFreeChestSlot uid = case uid of
  PlayerUnitId (PlayerId i) -> (players . ix i . object . playerUnitData . inventory) %= freeChestSlot
  MobUnitId (MobId i) -> (mobs . ix i . object . mobUnitData . inventory) %= freeChestSlot

unitFreeLegsSlot :: UnitId -> GameEnv ()
unitFreeLegsSlot uid = case uid of
  PlayerUnitId (PlayerId i) -> (players . ix i . object . playerUnitData . inventory) %= freeLegsSlot
  MobUnitId (MobId i) -> (mobs . ix i . object . mobUnitData . inventory) %= freeLegsSlot

unitFreeHandSlot :: UnitId -> GameEnv ()
unitFreeHandSlot uid = case uid of
  PlayerUnitId (PlayerId i) -> (players . ix i . object . playerUnitData . inventory) %= freeHandSlot
  MobUnitId (MobId i) -> (mobs . ix i . object . mobUnitData . inventory) %= freeHandSlot

setStrategy :: TaggedControl -> MobId -> FailableGameEnv UnitIdError ()
setStrategy tag (MobId i) = modify $ set (mobs . ix i . object . controlTag) tag
setStrategy _ _ = throwError UnitCastException

{- _getMobById :: Int -> FailableGameEnv UnitIdError Mob
_getMobById idx = do
  env <- get
  case env ^? mobs . ix idx . object of
    Nothing -> throwError InvalidUnitId
    Just mob -> return mob -}

{- setMobById :: Int -> Mob -> Environment -> Environment
setMobById idx mob env = env & mobs . ix idx . object .~ mob -}

{- getLevellingStats :: PlayerId -> FailableGameEnv UnitIdError LevellingStats
getLevellingStats (PlayerId i) = gets (view (player . object . levelling))
getLevellingStats _ = throwError UnitCastException -}

{- mobLensById :: Int -> Lens' Environment Mob
mobLensById idx = mobs . ix idx . _1 -}

-- | Apply UnitOp to the unit and change the environment.
affectUnit :: UnitId -> EUnitOp a -> FailableGameEnv UnitIdError a
affectUnit uid modifier = do
  unit <- gets (getUnitByUnitId uid) >>= liftEither
  let (newUnit, result) = applyUnitOp unit modifier
  _trySetUnit uid newUnit
  return result

{- getUnitPosition :: UnitId -> FailableGameEnv UnitIdError (Int, Int)
getUnitPosition uid = affectUnit uid UnitOp.getPosition -}

{- getUnitPortrait :: UnitId -> FailableGameEnv UnitIdError Char
getUnitPortrait uid = affectUnit uid UnitOp.getPortrait -}

{- getUnitStats :: UnitId -> FailableGameEnv UnitIdError Stats
getUnitStats uid = do
  maybeStats <- affectUnit uid UnitOp.getStats
  case maybeStats of
    Nothing -> throwError InvalidUnitId
    Just stats -> return stats -}

-- | Apply all modifiers to the unitId and get the result
queryUnitWithModifiers :: UnitId -> EUnitOp a -> Environment -> Either UnitIdError a
queryUnitWithModifiers uid modifier env = do
  unit <- getUnitByUnitId uid env
  let fact = env ^. modifierFactory
  let modifiedUnit = unitWithModifiers fact unit
  let (_, res) = applyUnitOp modifiedUnit modifier
  return res

-- | Get unit at position
unitAtPosition :: Position -> Environment -> Maybe UnitId
unitAtPosition position env = listToMaybe filtered
  where
    units = getActiveUnits env
    filtered = filterM (\u -> (== Right coord) <$> runExceptT (affectUnit u UnitOp.getPosition)) units

-- | Move unit to the position
moveUnit :: UnitId -> Position -> FailableGameEnv UnitIdError Bool
moveUnit uid pos = do
  isPassable <- gets (checkPassable uid pos) >>= liftEither
  if isPassable
    then affectUnit uid (UnitOp.setCoord pos) >> return True
    else return False

getUnitPosition :: UnitId -> Environment -> Either UnitIdError Position
getUnitPosition uid env = do
  unit <- getUnitByUnitId uid env
  return $ snd $ applyUnitOp unit UnitOp.getPosition

getLevelByUnitId :: UnitId -> Environment -> Either UnitIdError GameLevel
getLevelByUnitId uid env = do
  pos <- getUnitPosition uid env
  case Seq.lookup (pos ^. posLevel) (env ^. levels) of
    Nothing -> Left InvalidUnitId
    Just lvl -> return lvl

-- | Check whether a unit can exist on the position
checkPassable :: UnitId -> Position -> Environment -> Either UnitIdError Bool
checkPassable uid pos env = do
  unit <- getUnitByUnitId uid env
  let unitPos = snd $ applyUnitOp unit UnitOp.getPosition
  lvl <- getLevelByUnitId uid env
  let maybeCell = maybeGetCellAt (pos ^. posX, pos ^. posY) lvl
  let maybeStats = snd $ applyUnitOp unit UnitOp.getStats
  let isFree = isNothing $ unitAtPosition pos env
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
{- _accessUnit :: UnitId -> FailableGameEnv UnitIdError Unit
_accessUnit uid = do
  env <- get
  case uid of
    PlayerUnitId i -> return . MkPlayer $ (env ^. player . object)
    MobUnitId i -> do
      mob <- _getMobById i
      return $ MkMob mob -}

-- | Get Unit by the unitId. Fails if the unit is not present in the enviroment (e.g. was removed after death).
getUnitByUnitId :: UnitId -> Environment -> Either UnitIdError EUnit
getUnitByUnitId uid env = case uid of
  PlayerUnitId (PlayerId i) -> assert $ MkPlayer <$> (env ^? players . ix i . object)
  MobUnitId (MobId i) -> assert $ MkMob <$> (env ^? mobs . ix i . object)
  where
    assert Nothing = Left InvalidUnitId
    assert (Just x) = Right x

-- | Try to set Unit by UnitId.
_trySetUnit :: UnitId -> EUnit -> FailableGameEnv UnitIdError ()
_trySetUnit uid u =
  case (uid, u) of
    (PlayerUnitId (PlayerId i), MkPlayer p) -> players . ix i . object .= p
    (MobUnitId (MobId i), MkMob m) -> mobs . ix i . object .= m
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
  attacker <- liftEither $ getUnitByUnitId attackerId env
  attacked <- liftEither $ getUnitByUnitId attackedId env
  let (attacker', attacked') = attack fact attacker attacked
  _trySetUnit attackerId attacker' -- should always be true
  _trySetUnit attackedId attacked'
  return ()

evalAction :: UnitId -> Action -> FailableGameEnv UnitIdError Bool
evalAction u a = do
  env <- get
  case u of
    PlayerUnitId (PlayerId i) -> case (env ^? players . ix i . evaluator) of
      Nothing -> throwError InvalidUnitId
      Just unitEvaluator -> unitEvaluator a >> return True
    MobUnitId (MobId i) -> case env ^? mobs . ix i . evaluator of
      Nothing -> throwError InvalidUnitId
      Just unitEvaluator -> unitEvaluator a >> return True

{- getCurrentLevel :: Environment -> GameLevel
getCurrentLevel env = atDef (error "currentLevel index out of bounds") (env ^. levels) (env ^. currentLevel) -}

getAction :: MobId -> Environment -> Either UnitIdError (FailableGameEnv UnitIdError Action)
getAction mid@(MobId i) env = (env ^. strategy) <$> tag <*> pure mid
  where
    tag = case env ^? mobs . ix i . object . mobControlTag of
      Nothing -> throwError InvalidUnitId
      Just t -> return t

getVisibleToUnit :: UnitId -> Environment -> Either UnitIdError [Position]
getVisibleToUnit uid env = do
  pos <- getUnitPosition uid env
  unit <- getUnitByUnitId uid env
  lvl <- getLevelByUnitId uid env
  let stats = snd $ applyUnitOp unit UnitOp.getStats
  let visibility = getVisibility stats
  let createPosition (x, y) = makePosition env (pos ^. posLevel) x y
  case sequence $ map createPosition $ visiblePositions (lvl ^. lvlMap) visibility (positionXY pos) of
    Nothing -> error "Unreachable visible to player position"
    Just ps -> return ps

getSeenByPlayer :: PlayerId -> Environment -> Either UnitIdError SeenByPlayer
getSeenByPlayer (PlayerId i) env = case env ^? seenByPlayer . ix i of 
  Nothing -> Left InvalidUnitId
  Just sbp -> return sbp

{- getCells :: Environment -> (Array (Int, Int) Char)
getCells = do
  lvl <- getCurrentLevel
  return $ view (cellType . cellRender) <$> lvl ^. lvlMap . cells -}

updateSeenByPlayer :: PlayerId -> FailableGameEnv UnitIdError ()
updateSeenByPlayer pid@(PlayerId i) = do
  visible <- gets (getVisibleToUnit (PlayerUnitId pid)) >>= liftEither
  modify $ over (seenByPlayer . ix i) (bulkAddToSeenByPlayer visible)


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

makeTurn :: Action -> GameEnv ()
makeTurn playerAction = do
  player <- getPlayer
  _ <- runExceptT (evalAction player playerAction)
  mobs <- getActiveMobs
  _ <- runExceptT $ traverse (\u -> getAction u >>= evalAction u) mobs
  units <- getActiveUnits
  _ <- runExceptT $ traverse (`affectUnit` tickTimedEffects) units
  return ()

{--------------------------------------------------------------------
  Save / Load
--------------------------------------------------------------------}

data EnvMemento
  = EnvMemento
      { envPlayers :: [EPlayer],
        envMobs :: [(Int, EMob)],
        envLevels :: [GameLevel],
        envGen :: StdGen,
        envSeen :: [Set.Set (Int, Int)]
      }
  deriving (Generic)

getEnvState :: Environment -> EnvMemento
getEnvState env =
  EnvMemento
    { envPlayers = env ^. players . object,
      envMobs = itoList $ IntMap.map _object $ env ^. mobs,
      envLevels = _levels env,
      envGen = _randomGenerator env,
      envSeen = _seenByPlayer env
    }

loadEnvironmentState (EnvMemento player imobs levels gen seen) =
  makeEnvironmentInternal
    player
    imobs
    levels
    gen
    seen
