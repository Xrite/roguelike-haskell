{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Environment
  ( Environment,
    UnitId,
    GameEnv,
    makeEnvironment,
    getCurrentLevel,
    unitByCoord,
    envAttack,
    playerId,
    affectUnit,
    moveUnit,
    runGameEnv,
    renderEnvironment,
    evalAction,
  )
where

import Control.Lens hiding (levels)
import Control.Monad.State
import Control.Monad.Fail
import Data.Foldable (find)
import Data.List (findIndex)
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Game.ActionEvaluation
import Game.GameLevels.GameLevel
import Game.GameLevels.MapCell
import Game.GameLevels.MapCellType
import Game.Modifiers.UnitOp as UnitOp
import Game.Modifiers.UnitOpFactory (UnitOpFactory)
import Game.Unit.DamageCalculation (attack)
import Game.Unit.Stats
import Game.Unit.Unit
import Game.Unit.Action
import PreludeUtil (listLens)

-- | All manipulations with units in environment should use this type
data UnitId = MobUnitId Int | PlayerUnitId

-- TODO maybe extract units to a different module?
-- TODO comment

-- | Contains description of current game state (e.g. map, mobs)
data Environment
  = Environment
      { _player :: Player,
        _mobs :: [(Int, Mob GameEnv)],
        _levels :: [GameLevel],
        _currentLevel :: Int,
        _currentUnitTurn :: Int,
        _modifierFactory :: UnitOpFactory,
        _playerEvaluator :: Action -> GameEnv (),
        _mobEvaluators :: [Action -> GameEnv ()]
      }

-- | A type for evaluating action on Environment
newtype GameEnv a = GameEnv {unGameEnv :: State Environment a} deriving (Functor, Applicative, Monad, MonadState Environment)

makeLenses ''Environment

instance MonadFail GameEnv where
  fail = error

runGameEnv :: GameEnv a -> Environment -> (a, Environment)
runGameEnv gameEnv = runState (unGameEnv gameEnv)

instance Show Environment where
  show _ = "Environment"

-- | Constructs a new 'Environment'.
makeEnvironment :: Player -> [Mob GameEnv] -> [GameLevel] -> UnitOpFactory -> Environment
makeEnvironment player mobs levels factory =
  Environment
    { _player = player,
      _mobs = zip [0 ..] mobs,
      _levels = levels,
      _currentLevel = 0,
      _currentUnitTurn = 0,
      _modifierFactory = factory,
      _playerEvaluator = defaultEvaluation PlayerUnitId,
      _mobEvaluators = [const $ return ()]
    }

-- | This function should remove dead units from environment.
-- It is called after each function that can modify units in the environment. With current implementation of units storage it invalidates 'UnitId'.
-- Item drop (units death modifiers in general) is not yet implemented, so TODO implement death modifiers in filterDead
filterDead :: GameEnv ()
filterDead = do
  env <- get
  modify $ set mobs (newMobs env)
  where
    newMobs env = filter (isAlive . snd) (env ^. mobs)

getActiveUnits :: GameEnv [UnitId]
getActiveUnits = do
  filterDead
  env <- get
  let players = if isAlive (env ^. player) then [PlayerUnitId] else []
  let activeMobs = map (MobUnitId . fst) (env ^. mobs)
  return $ players ++ activeMobs

{- unitLensById :: UnitId -> Lens' Environment Unit.AnyUnit
unitLensById (UnitId idxInt) = units . listLens idxInt -}

{- unitById :: UnitId -> Environment -> Unit.AnyUnit
unitById idx env = env ^. unitLensById idx -}

{- setUnitById :: UnitId -> Unit.AnyUnit -> Environment -> Environment
setUnitById idx unit = filterDead . set (unitLensById idx) unit -}

affectUnit :: UnitId -> UnitOp a -> GameEnv a
affectUnit PlayerUnitId modifier = do
  env <- get
  let (newPlayer, result) = applyUnitOp modifier (env ^. player)
  modify $ set player newPlayer
  filterDead
  return result
affectUnit (MobUnitId idx) modifier = do
  env <- get
  let (newMob, result) = applyUnitOp modifier (snd $ (env ^. mobs) !! idx)
  modify $ set (mobs . ix idx . _2) newMob
  filterDead
  return result

unitByCoord :: (Int, Int) -> GameEnv (Maybe UnitId)
unitByCoord coord = do
  units <- getActiveUnits
  filtered <- filterM (\u -> (== coord) <$> affectUnit u UnitOp.getPosition) units
  return $ listToMaybe filtered

moveUnit :: UnitId -> (Int, Int) -> GameEnv Bool
moveUnit u pos = do
  isPassable <- checkPassable u pos
  if isPassable
    then affectUnit u $ UnitOp.setCoord pos >> return True
    else return False

checkPassable :: UnitId -> (Int, Int) -> GameEnv Bool
checkPassable uid pos = do
  lvl <- getCurrentLevel
  unitPos <- affectUnit uid UnitOp.getPosition 
  let maybeCell = maybeGetCellAt pos lvl
  maybeStats <- affectUnit uid UnitOp.getStats
  isFree <- isNothing <$> unitByCoord pos
  if pos == unitPos || (isJust $ checkAll maybeCell maybeStats isFree)
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

-- | Get AnyUnit from UnitId
_accessUnit :: UnitId -> GameEnv (AnyUnit GameEnv)
_accessUnit uid = do
  env <- get
  case uid of
    PlayerUnitId -> return $ MkPlayer (env ^. player)
    MobUnitId i -> return $ MkMob (snd $ (env ^. mobs) !! i)

-- | Try to set AnyUnit by Unit id. Return True when success.
_trySetUnit :: UnitId -> AnyUnit GameEnv -> GameEnv Bool
_trySetUnit uid u = case (uid, u) of
  (PlayerUnitId, MkPlayer p) -> modify (set player p) >> return True
  (MobUnitId i, MkMob m) -> modify (set (mobs . ix i . _2) m) >> return True
  _ -> return False

-- | Perform and attack between two units
envAttack ::
  -- | Attacker
  UnitId ->
  -- | Attacked
  UnitId ->
  GameEnv ()
envAttack attackerId attackedId = do
  env <- get
  let fact = env ^. modifierFactory
  attacker <- _accessUnit attackerId
  attacked <- _accessUnit attackedId
  let (attacker', attacked') = attack fact attacker attacked
  True <- _trySetUnit attackerId attacker'
  True <- _trySetUnit attackedId attacked'
  return ()

evalAction :: UnitId -> Action -> GameEnv ()
evalAction u a = do
  env <- get
  case u of
    PlayerUnitId -> (env ^. playerEvaluator) a
    MobUnitId idx -> fromMaybe (return ()) $ (env ^? mobEvaluators . ix idx) <*> pure a

getCurrentLevel :: GameEnv GameLevel
getCurrentLevel = do
  env <- get
  return $ (env ^. levels) !! (env ^. currentLevel)

playerId :: Environment -> UnitId
playerId _ = PlayerUnitId

renderEnvironment :: GameEnv [String]
renderEnvironment = do
  lvl <- getCurrentLevel
  let mp = lvl ^. lvlMap
  let ((xFrom, yFrom), (xTo, yTo)) = getMapSize mp
  let terrain = [[renderCell $ getCell (x, y) mp | x <- [xFrom .. xTo]] | y <- [yFrom .. yTo]]
  units <- getActiveUnits
  positions <- traverse (`affectUnit` UnitOp.getPosition) units
  portraits <- traverse (`affectUnit` UnitOp.getPortrait) units
  return $ foldl (\m ((x, y), p) -> m & ix y . ix x .~ p) terrain (zip positions portraits)
