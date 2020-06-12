{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Game.Environment
  ( Environment,
    UnitId,
    PlayerId,
    MobId,
    GameEnv,
    FallibleGameEnv,
    UnitIdError (..),
    Position,
    SeenByPlayer,
    makeEnvironment,
    makePlayerId,
    queryUnitWithModifiers,
    runGameEnv,
    randomRGameEnv,
    getActiveMobs,
    getAction,
    getActiveUnits,
    getActivePlayers,
    getActiveMobs,
    getVisibleToUnit,
    getSeenByPlayer,
    EnvMemento,
    getEnvState,
    loadEnvironmentState,
    getUnitInventory,
    unitFreeChestSlot,
    unitFreeHandSlot,
    unitFreeHeadSlot,
    unitFreeLegsSlot,
    evalAction,
    getCurrentUnit,
    popCurrentUnit,
    addUnitToQueue,
    affectUnit,
    unitEquipItem,
    getUnitByUnitId,
    getPlayerByPlayerId,
    getMobByMobId,
    getUnitPosition,
    getUnitPortrait,
    seenAtLevel,
    getLevelByUnitId,
    positionXY,
    getSeenByPlayer,
    posLevel,
    posX,
    posY,
    isUnitAlive,
    updateSeenByPlayer,
    getFreePositionsOnLevel,
    addPlayerToEnvironment,
    addMobToEnvironment,
    removePlayerFromEnvironment,
    removeMobFromEnvironment,
  )
where


import Control.Lens hiding (levels, (<|), (|>))
import Control.Monad (void, when)
import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.State
import Data.Array
import Data.Foldable (toList)
import Data.Either (rights)
import qualified Data.IntMap as IntMap
import Data.List
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Sequence (Seq (Empty, (:<|), (:|>)), (<|), (|>))
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
import Game.Position
import Debug.Trace

-- | All manipulations with units in environment should use this type
data UnitId
  = MobUnitId MobId
  | PlayerUnitId PlayerId
  deriving (Eq, Generic, Show)

newtype MobId = MobId Int
  deriving (Eq, Show, Generic)

newtype PlayerId = PlayerId Int
  deriving (Eq, Show, Generic)

-- | Should obey cast . downcast = Just
class a `Is` b where
  cast :: a -> b
  downcast :: b -> Maybe a

instance a `Is` a where
  cast = id
  
  downcast = Just

instance MobId `Is` UnitId where
  cast = MobUnitId 

  downcast (MobUnitId mid) = Just mid
  downcast _ = Nothing

instance PlayerId `Is` UnitId where
  cast = PlayerUnitId 

  downcast (PlayerUnitId pid) = Just pid
  downcast _ = Nothing

-- TODO maybe extract units to a different module?
-- TODO comment

type EPlayer = Player

type EMob = Mob 

type EUnit = Unit 

type EUnitOp a = UnitOp a

type EUnitOpFactory = UnitOpFactory 

instance Eq StdGen where
  g1 == g2 = show g1 == show g2

-- | All positions in the environment have been seen by a player
newtype SeenByPlayer = SeenByPlayer (IntMap.IntMap (Set.Set (Int, Int)))
  deriving (Generic, Show)


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
        _strategy :: TaggedControl -> MobId -> FallibleGameEnv UnitIdError Action,
        _seenByPlayer :: IntMap.IntMap SeenByPlayer
      }
  deriving (Generic)


-- | A box for a unit with it's evaluator.
data WithEvaluator a
  = WithEvaluator
      { _object :: a,
        _evaluator :: Action -> FallibleGameEnv UnitIdError ()
      }
  deriving (Generic)

-- | A type for evaluating action on Environment
newtype GameEnv a = GameEnv {unGameEnv :: State Environment a} 
  deriving (Functor, Applicative, Monad, MonadState Environment)

-- | An error that could occur when evaluating actions on environment
data UnitIdError
  = InvalidUnitId
  | NoSuchUnit
  | UnitCastException
  | WrongUnitTurn
  deriving (Eq)

type FallibleGameEnv err = ExceptT err GameEnv

makeLenses ''Environment

makeLenses ''WithEvaluator

instance MonadFail GameEnv where
  fail = error

instance Show Environment where
  show _ = "Environment"

makePlayerId = PlayerId

makeMobId = MobId

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

levelsCount :: Environment -> Int
levelsCount env = env ^. levels & Seq.length

emptySeenByPlayer :: SeenByPlayer
emptySeenByPlayer = SeenByPlayer IntMap.empty

makeSeenByPlayer :: [Position] -> SeenByPlayer
makeSeenByPlayer positions = foldl' addToSeenByPlayer emptySeenByPlayer positions

addToSeenByPlayer :: SeenByPlayer -> Position -> SeenByPlayer
addToSeenByPlayer (SeenByPlayer s) p = 
  SeenByPlayer $ IntMap.insertWith Set.union (p ^. posLevel) (Set.singleton (positionXY p)) s

bulkAddToSeenByPlayer :: Foldable t => t Position -> SeenByPlayer -> SeenByPlayer
bulkAddToSeenByPlayer ps s = foldl' addToSeenByPlayer s ps

seenAtLevel :: Int -> SeenByPlayer -> Set.Set (Int, Int)
seenAtLevel l (SeenByPlayer s) = fromMaybe Set.empty (IntMap.lookup l s)

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
  
-- | All position available to player
getFreePositionsOnLevel :: Environment -> Stats -> Int -> [Position]
getFreePositionsOnLevel env st l = case env ^? levels . ix l of
  Nothing -> []
  Just lvl -> 
    let ((xFrom, yFrom), (xTo, yTo)) = getMapSize (lvl ^. lvlMap) in
    let allCellPositions = mapMaybe (passableCell lvl) [(x, y) | x <- [xFrom..xTo], y <- [yFrom..yTo]] in
    let occupiedPositions = rights $ map (\uid -> getUnitPosition uid env) (getActiveUnits env) in
    allCellPositions \\ occupiedPositions
  where
    passableCell lvl p = do
      cell <- maybeGetCellAt p lvl
      guard $ (cell ^. cellType . passable) st
      return $ uncheckedPosition l p

freePlayerId :: Environment -> PlayerId
freePlayerId env = let Just idx = find isFree [1..] in PlayerId idx
  where 
    isFree idx = IntMap.notMember idx (env ^. players)

freeMobId :: Environment -> MobId
freeMobId env = let Just idx = find isFree [1..] in MobId idx
  where 
    isFree idx = IntMap.notMember idx (env ^. mobs)

-- | Add player to the environment. It does not add player to the queue.
addPlayerToEnvironment :: Player -> GameEnv PlayerId
addPlayerToEnvironment p = do
  pid <- gets freePlayerId
  let with = WithEvaluator p $ defaultEvaluation $ cast pid
  let (PlayerId i) = pid
  modify $ over players (IntMap.insert i with)
  return pid

-- | Add mob to the environment. It does not add mob to the queue.
addMobToEnvironment :: Mob -> GameEnv MobId
addMobToEnvironment m = do
  mid <- gets freeMobId
  let with = WithEvaluator m $ defaultEvaluation $ cast mid
  let (MobId i) = mid
  modify $ over mobs (IntMap.insert i with)
  return mid

-- | Remove player from the environment. Also removes player from the queue
removePlayerFromEnvironment :: PlayerId -> GameEnv ()
removePlayerFromEnvironment pid = do
  let (PlayerId i) = pid
  modify $ over players (IntMap.delete i)
  modify $ over unitQueue (Seq.filter (/= cast pid))

-- | Remove mob from the environment. Also removes mob from the queue
removeMobFromEnvironment :: MobId -> GameEnv ()
removeMobFromEnvironment mid = do
  let (MobId i) = mid
  modify $ over mobs (IntMap.delete i)
  modify $ over unitQueue (Seq.filter (/= cast mid))

{- getActivePlayer :: FallibleGameEnv UnitIdError UnitId
getActivePlayer = do
  env <- get
  if isAlive (env ^. player . object)
    then return PlayerUnitId
    else throwError NoSuchUnit -}

{- isPlayerAlive :: Environment -> Bool
isPlayerAlive env = isAlive (env ^. player . object) -}

-- | Checks whether a unit is still alive. Returns False if the unit is not present in the environment.
isUnitAlive :: (uid `Is` UnitId) => uid -> Environment -> Bool
isUnitAlive uid env = case cast uid of
  PlayerUnitId (PlayerId i) -> IntMap.member i (env ^. players)
  MobUnitId (MobId i) -> IntMap.member i (env ^. mobs)

{- getPlayer :: GameEnv UnitId
getPlayer = return PlayerUnitId -}

getUnitInventory :: (uid `Is` UnitId) => uid -> Environment -> Either UnitIdError Inventory
getUnitInventory uid env = case cast uid of 
  PlayerUnitId (PlayerId i) -> assert $ env ^? players . ix i . object . playerUnitData . inventory
  MobUnitId (MobId i) -> assert $ env ^? mobs . ix i . object . mobUnitData . inventory
  where
    assert Nothing = Left InvalidUnitId
    assert (Just x) = Right x

-- | Sets an inventory to the unit. Can fail if unit is not present in the environment.
setUnitInventory :: (uid `Is` UnitId) => uid -> Inventory -> FallibleGameEnv UnitIdError ()
setUnitInventory uid inv = case cast uid of
  PlayerUnitId (PlayerId i) -> modify $ set (players . ix i . object . playerUnitData . inventory) inv
  MobUnitId (MobId i) -> modify $ set (mobs . ix i . object . mobUnitData . inventory) inv

unitEquipItem :: (uid `Is` UnitId) => uid -> Int -> FallibleGameEnv UnitIdError ()
unitEquipItem uid i = do
  unit <- gets (getUnitByUnitId uid) >>= liftEither
  let inv = getUnitData unit ^. inventory
  case tryEquipItem i inv of
    (Right inv') -> setUnitInventory uid inv'
    (Left _) -> return ()

unitFreeHeadSlot :: (uid `Is` UnitId) => uid -> GameEnv ()
unitFreeHeadSlot uid = case cast uid of
  PlayerUnitId (PlayerId i) -> (players . ix i . object . playerUnitData . inventory) %= freeHeadSlot
  MobUnitId (MobId i) -> (mobs . ix i . object . mobUnitData . inventory) %= freeHeadSlot

unitFreeChestSlot :: (uid `Is` UnitId) => uid -> GameEnv ()
unitFreeChestSlot uid = case cast uid of
  PlayerUnitId (PlayerId i) -> (players . ix i . object . playerUnitData . inventory) %= freeChestSlot
  MobUnitId (MobId i) -> (mobs . ix i . object . mobUnitData . inventory) %= freeChestSlot

unitFreeLegsSlot :: (uid `Is` UnitId) => uid -> GameEnv ()
unitFreeLegsSlot uid = case cast uid of
  PlayerUnitId (PlayerId i) -> (players . ix i . object . playerUnitData . inventory) %= freeLegsSlot
  MobUnitId (MobId i) -> (mobs . ix i . object . mobUnitData . inventory) %= freeLegsSlot

unitFreeHandSlot :: (uid `Is` UnitId) => uid -> GameEnv ()
unitFreeHandSlot uid = case cast uid of
  PlayerUnitId (PlayerId i) -> (players . ix i . object . playerUnitData . inventory) %= freeHandSlot
  MobUnitId (MobId i) -> (mobs . ix i . object . mobUnitData . inventory) %= freeHandSlot

setStrategy :: TaggedControl -> MobId -> FallibleGameEnv UnitIdError ()
setStrategy tag (MobId i) = modify $ set (mobs . ix i . object . mobControlTag) tag
setStrategy _ _ = throwError UnitCastException

{- _getMobById :: Int -> FallibleGameEnv UnitIdError Mob
_getMobById idx = do
  env <- get
  case env ^? mobs . ix idx . object of
    Nothing -> throwError InvalidUnitId
    Just mob -> return mob -}

{- setMobById :: Int -> Mob -> Environment -> Environment
setMobById idx mob env = env & mobs . ix idx . object .~ mob -}

{- getLevellingStats :: PlayerId -> FallibleGameEnv UnitIdError LevellingStats
getLevellingStats (PlayerId i) = gets (view (player . object . levelling))
getLevellingStats _ = throwError UnitCastException -}

{- mobLensById :: Int -> Lens' Environment Mob
mobLensById idx = mobs . ix idx . _1 -}

-- | Apply UnitOp to the unit and change the environment.
affectUnit :: (uid `Is` UnitId) => uid -> EUnitOp a -> FallibleGameEnv UnitIdError a
affectUnit uid modifier = do
  unit <- gets (getUnitByUnitId uid) >>= liftEither
  let (newUnit, result) = applyUnitOp unit modifier
  _trySetUnit uid newUnit
  return result

{- getUnitPosition :: UnitId -> FallibleGameEnv UnitIdError (Int, Int)
getUnitPosition uid = affectUnit uid UnitOp.getPosition -}

{- getUnitPortrait :: UnitId -> FallibleGameEnv UnitIdError Char
getUnitPortrait uid = affectUnit uid UnitOp.getPortrait -}

{- getUnitStats :: UnitId -> FallibleGameEnv UnitIdError Stats
getUnitStats uid = do
  maybeStats <- affectUnit uid UnitOp.getStats
  case maybeStats of
    Nothing -> throwError InvalidUnitId
    Just stats -> return stats -}

-- | Apply all modifiers to the unitId and get the result
queryUnitWithModifiers :: (uid `Is` UnitId) => uid -> EUnitOp a -> Environment -> Either UnitIdError a
queryUnitWithModifiers uid modifier env = do
  unit <- getUnitByUnitId uid env
  let fact = env ^. modifierFactory
  let modifiedUnit = unitWithModifiers fact unit
  let (_, res) = applyUnitOp modifiedUnit modifier
  return res

-- | Get unit at position
unitAtPosition :: Position -> Environment -> Maybe UnitId
unitAtPosition pos env = listToMaybe allAtPos
  where
    units = getActiveUnits env
    allAtPos = case filterM (\u -> (pos ==) <$> getUnitPosition u env) units of
      Left _ -> error "failure at unitAtPosition"
      Right r -> r

-- | Move unit to the position
moveUnit :: (uid `Is` UnitId) => uid -> Position -> FallibleGameEnv UnitIdError Bool
moveUnit uid pos = do
  isPassable <- gets (checkPassable uid pos) >>= liftEither
  if isPassable
    then affectUnit uid (UnitOp.setCoord pos) >> return True
    else return False

getUnitPosition :: (uid `Is` UnitId) => uid -> Environment -> Either UnitIdError Position
getUnitPosition uid env = do
  unit <- getUnitByUnitId uid env
  return $ snd $ applyUnitOp unit UnitOp.getPosition

getUnitPortrait :: (uid `Is` UnitId) => uid -> Environment -> Either UnitIdError Char
getUnitPortrait uid env = do
  unit <- getUnitByUnitId uid env
  return $ snd $ applyUnitOp unit UnitOp.getPortrait

getLevelByUnitId :: (uid `Is` UnitId) => uid -> Environment -> Either UnitIdError GameLevel
getLevelByUnitId uid env = do
  pos <- getUnitPosition uid env
  case Seq.lookup (pos ^. posLevel) (env ^. levels) of
    Nothing -> Left InvalidUnitId
    Just lvl -> return lvl

-- | Check whether a unit can exist on the position
checkPassable :: (uid `Is` UnitId) => uid -> Position -> Environment -> Either UnitIdError Bool
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
{- _accessUnit :: UnitId -> FallibleGameEnv UnitIdError Unit
_accessUnit uid = do
  env <- get
  case uid of
    PlayerUnitId i -> return . MkPlayer $ (env ^. player . object)
    MobUnitId i -> do
      mob <- _getMobById i
      return $ MkMob mob -}

-- | Returns a first unit from the unit queue
getCurrentUnit :: Environment -> Maybe UnitId
getCurrentUnit env = case (env ^. unitQueue) of
  Seq.Empty -> Nothing
  uid :<| _ -> Just uid

-- | Returns a first unit from the unit queue and removes it
popCurrentUnit :: GameEnv (Maybe UnitId)
popCurrentUnit = do
  currentUnit <- gets getCurrentUnit
  modify $ over unitQueue (Seq.drop 1)
  return currentUnit

-- | Add a unit to the end of the unit queue
addUnitToQueue :: (uid `Is` UnitId) => uid -> GameEnv ()
addUnitToQueue uid = do
  modify $ over unitQueue (|> cast uid)


-- | Get Unit by the unitId. Fails if the unit is not present in the environment (e.g. was removed after death).
getUnitByUnitId :: (uid `Is` UnitId) => uid -> Environment -> Either UnitIdError EUnit
getUnitByUnitId uid env = case cast uid of
  PlayerUnitId pid -> MkPlayer <$> getPlayerByPlayerId pid env
  MobUnitId mid -> MkMob <$> getMobByMobId mid env

getPlayerByPlayerId :: PlayerId -> Environment -> Either UnitIdError EPlayer
getPlayerByPlayerId (PlayerId i) env = case (env ^? players . ix i . object) of
  Nothing -> Left InvalidUnitId
  Just p -> Right p

getMobByMobId :: MobId -> Environment -> Either UnitIdError EMob
getMobByMobId (MobId i) env = case (env ^? mobs  . ix i . object) of
  Nothing -> Left InvalidUnitId
  Just m -> Right m

-- | Try to set Unit by UnitId.
_trySetUnit :: (uid `Is` UnitId) => uid -> EUnit -> FallibleGameEnv UnitIdError ()
_trySetUnit uid u =
  case (cast uid, u) of
    (PlayerUnitId (PlayerId i), MkPlayer p) -> players . ix i . object .= p
    (MobUnitId (MobId i), MkMob m) -> mobs . ix i . object .= m
    _ -> throwError UnitCastException

-- | Perform and attack between two units
envAttack ::
  (uid `Is` UnitId) => uid -> uid -> FallibleGameEnv UnitIdError ()
envAttack attackerId attackedId = do
  env <- get
  let fact = env ^. modifierFactory
  attacker <- liftEither $ getUnitByUnitId attackerId env
  attacked <- liftEither $ getUnitByUnitId attackedId env
  let (attacker', attacked') = attack fact attacker attacked
  _trySetUnit attackerId attacker' -- should always be true
  _trySetUnit attackedId attacked'
  return ()

evalAction :: (uid `Is` UnitId) => uid -> Action -> FallibleGameEnv UnitIdError Bool
evalAction uid a = do
  env <- get
  case cast uid of
    PlayerUnitId (PlayerId i) -> case (env ^? players . ix i . evaluator) of
      Nothing -> throwError InvalidUnitId
      Just unitEvaluator -> unitEvaluator a >> return True
    MobUnitId (MobId i) -> case env ^? mobs . ix i . evaluator of
      Nothing -> throwError InvalidUnitId
      Just unitEvaluator -> unitEvaluator a >> return True

{- getCurrentLevel :: Environment -> GameLevel
getCurrentLevel env = atDef (error "currentLevel index out of bounds") (env ^. levels) (env ^. currentLevel) -}

getAction :: MobId -> Environment -> Either UnitIdError (FallibleGameEnv UnitIdError Action)
getAction mid@(MobId i) env = (env ^. strategy) <$> tag <*> pure mid
  where
    tag = case env ^? mobs . ix i . object . mobControlTag of
      Nothing -> throwError InvalidUnitId
      Just t -> return t

getVisibleToUnit :: (uid `Is` UnitId) => uid -> Environment -> Either UnitIdError [Position]
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

updateSeenByPlayer :: PlayerId -> FallibleGameEnv UnitIdError ()
updateSeenByPlayer pid@(PlayerId i) = do
  visible <- gets (getVisibleToUnit pid) >>= liftEither
  --traceShowM visible
  modify $ over (seenByPlayer . ix i) (bulkAddToSeenByPlayer visible)


--------------------------------------------------------------------
-- Control
--------------------------------------------------------------------

getControl :: TaggedControl -> MobId -> FallibleGameEnv UnitIdError Action
getControl Aggressive = aggressiveControl
getControl (Passive dest) = passiveControl dest
getControl Avoiding = avoidingControl
getControl DoNothing = const $ return stayAtPosition

allVisiblePlayersXYToMob :: MobId -> FallibleGameEnv UnitIdError [(Int, Int)] 
allVisiblePlayersXYToMob mid = do
  env <- get
  mobPosition <- gets (getUnitPosition (MobUnitId mid)) >>= liftEither
  currentLevel <- liftEither $ getLevelByUnitId mid env
  playersOnSameLevel <- gets (getActivePlayers) >>= filterM (checkSameLevel (mobPosition ^. posLevel))
  playerXYs <- map positionXY <$> (liftEither $ mapM (\p -> getUnitPosition p env) playersOnSameLevel)
  maybeMobStats <- affectUnit mid UnitOp.getStats
  let visiblePlayerXYs = filter (canSee (currentLevel ^. lvlMap) (getVisibility maybeMobStats) (positionXY mobPosition)) playerXYs 
  return visiblePlayerXYs
  where
    checkSameLevel lvl u = do
      pos <- gets (getUnitPosition u) >>= liftEither
      if pos ^. posLevel == lvl then return True else return False

allUnitsXYAtTheSameLevel ::
  MobId ->
  FallibleGameEnv UnitIdError [(Int, Int)]
allUnitsXYAtTheSameLevel mid = do
  env <- get
  mobPosition <- gets (getUnitPosition (MobUnitId mid)) >>= liftEither
  unitsOnSameLevel <- gets (getActiveUnits) >>= filterM (checkSameLevel (mobPosition ^. posLevel))
  allXYs <- map positionXY <$> (liftEither $ mapM (\u -> getUnitPosition u env) unitsOnSameLevel)
  return allXYs
  where
    checkSameLevel lvl u = do
      pos <- gets (getUnitPosition u) >>= liftEither
      if pos ^. posLevel == lvl then return True else return False


-- | Tries to reach and attack a player whenever player is visible to this unit
aggressiveControl :: MobId -> FallibleGameEnv UnitIdError Action
aggressiveControl mid = do
  mobPos <- gets (getUnitPosition (MobUnitId mid)) >>= liftEither
  playersOnSameLevel <- gets (getActivePlayers) >>= filterM (checkSameLevel (mobPos ^. posLevel))
  unitsOnSameLevel <- gets (getActiveUnits) >>= filterM (checkSameLevel (mobPos ^. posLevel))
  env <- get
  playerXYs <- map positionXY <$> (liftEither $ mapM (\p -> getUnitPosition p env) playersOnSameLevel)
  allXYs <- map positionXY <$> (liftEither $ mapM (\u -> getUnitPosition u env) unitsOnSameLevel)
  lvl <- liftEither $ getLevelByUnitId mid env
  maybeMobStats <- affectUnit mid UnitOp.getStats
  let visiblePlayerXYs = filter (canSee (lvl ^. lvlMap) (getVisibility maybeMobStats) (positionXY mobPos)) playerXYs 
  let path = findNearest (lvl ^. lvlMap) (getPassability maybeMobStats) (positionXY mobPos) visiblePlayerXYs (allXYs \\ ([positionXY mobPos] ++ visiblePlayerXYs))
  case path of
      Nothing -> return stayAtPosition
      Just (_, (_ : (x, y) : _)) -> return $ deltaToAction (x - (mobPos ^. posX), y - (mobPos ^. posY))
      Just _ -> return stayAtPosition
  where
    checkSameLevel lvl u = do
      pos <- gets (getUnitPosition u) >>= liftEither
      if pos ^. posLevel == lvl then return True else return False

passiveControl :: (Int, Int) -> MobId -> FallibleGameEnv UnitIdError Action
passiveControl dest mid = do
  mobPosition <- gets (getUnitPosition mid) >>= liftEither
  if positionXY mobPosition == dest
    then generateNewDestination mobPosition
    else advanceToDestination mobPosition
  where
    generateNewDestination mobPosition = do
      currentLevel <- gets (getLevelByUnitId mid) >>= liftEither
      maybeStats <- affectUnit mid UnitOp.getStats
      allXYs <- allUnitsXYAtTheSameLevel mid
      let mobXY = positionXY mobPosition
      let allVisible = visiblePositions (currentLevel ^. lvlMap) (getVisibility maybeStats) mobXY
      let available = mobXY : (allVisible \\ allXYs)
      randomIndex <- lift $ randomRGameEnv (0, length available - 1)
      let newDestination = atDef (error $ "wrong available position index: " ++ show randomIndex ++ " out of " ++ show (length available)) available randomIndex
      setStrategy (Passive newDestination) mid
      return stayAtPosition

    advanceToDestination mobPosition = do
      currentLevel <- gets (getLevelByUnitId mid) >>= liftEither
      maybeStats <- affectUnit mid UnitOp.getStats
      allXYs <- allUnitsXYAtTheSameLevel mid
      let mobXY = positionXY mobPosition
      let path = findPath (getPassability maybeStats) (currentLevel ^. lvlMap) mobXY dest (allXYs \\ [mobXY])
      case path of
        Nothing -> generateNewDestination mobPosition
        Just (_ : (x, y) : _) -> return $ deltaToAction (x - fst mobXY, y - snd mobXY)
        Just _ -> return stayAtPosition

avoidingControl :: MobId -> FallibleGameEnv UnitIdError Action
avoidingControl mid = do
  env <- get
  mobPosition <- gets (getUnitPosition mid) >>= liftEither
  currentLevel <- gets (getLevelByUnitId mid) >>= liftEither
  maybeStats <- affectUnit mid UnitOp.getStats
  playerXYs <- allVisiblePlayersXYToMob mid
  allXYs <- allUnitsXYAtTheSameLevel mid
  let mobXY = positionXY mobPosition
  let possible = getFurthestFrom (currentLevel ^. lvlMap) (getPassability maybeStats) mobXY allXYs (allXYs \\ [mobXY])
  case possible of
    [] -> return stayAtPosition
    xs -> do
      randomIndex <- lift $ randomRGameEnv (0, length xs - 1)
      let (x, y) = xs !! randomIndex
      return $ deltaToAction (x - fst mobXY, y - snd mobXY)
  where
    distance2 (x1, y1) (x2, y2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2

getPassability maybeStats cell = case maybeStats of
  Nothing -> False
  Just stats -> (cell ^. cellType . passable) stats

getVisibility maybeStats cell = case maybeStats of
  Nothing -> False
  Just stats -> (cell ^. cellType . transparent) stats

type ActionEvaluator = UnitId -> Action -> FallibleGameEnv UnitIdError ()

basicEvaluation :: ActionEvaluator
basicEvaluation uid (Move xDir yDir) = do
  pos <- affectUnit uid UnitOp.getPosition
  let (x', y') = Action.changeCoord xDir yDir (positionXY pos)
  let newPos = Position (pos ^. posLevel) x' y'
  unitAtPos <- gets (unitAtPosition newPos) 
  case unitAtPos of
    Nothing -> void $ moveUnit uid newPos
    Just other -> when (other /= uid) $ envAttack uid other

defaultEvaluation :: ActionEvaluator
defaultEvaluation = confuseAwareDecorator basicEvaluation

confuseAwareDecorator :: ActionEvaluator -> ActionEvaluator
confuseAwareDecorator eval uid dir = do
  isConfused <- gets (queryUnitWithModifiers uid UnitOp.getConfusion) >>= liftEither
  let eval' =
        if isConfused
          then confusedDecorator eval
          else eval
  eval' uid dir

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

{- makeTurn :: Action -> GameEnv ()
makeTurn playerAction = do
  player <- getPlayer
  _ <- runExceptT (evalAction player playerAction)
  mobs <- getActiveMobs
  _ <- runExceptT $ traverse (\u -> getAction u >>= evalAction u) mobs
  units <- getActiveUnits
  _ <- runExceptT $ traverse (`affectUnit` tickTimedEffects) units
  return () -}


{--------------------------------------------------------------------
  Save / Load
--------------------------------------------------------------------}

data EnvMemento
  = EnvMemento
      { envPlayers :: [(Int, EPlayer)],
        envMobs :: [(Int, EMob)],
        envLevels :: [GameLevel],
        envGen :: StdGen,
        envSeen :: [(Int, SeenByPlayer)]
      }
  deriving (Generic)

getEnvState :: Environment -> EnvMemento
getEnvState env =
  EnvMemento
    { envPlayers = IntMap.toList $ IntMap.map _object $ env ^. players,
      envMobs = IntMap.toList $ IntMap.map _object $ env ^. mobs,
      envLevels = toList $ env ^. levels,
      envGen =  env ^. randomGenerator,
      envSeen = IntMap.toList $ env ^. seenByPlayer
    }

loadEnvironmentState :: EnvMemento -> Environment
loadEnvironmentState (EnvMemento player imobs levels gen seen) =
  makeEnvironmentInternal
    player
    imobs
    levels
    gen
    seen
