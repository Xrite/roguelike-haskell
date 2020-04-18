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
import Data.Foldable (find)
import Data.List (findIndex)
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Game.GameLevels.GameLevel
import Game.GameLevels.MapCell (renderCell)
import Game.Modifiers.Modifier as Modifier
import Game.Modifiers.ModifierFactory (ModifierFactory)
import Game.Unit.DamageCalculation (attack)
import Game.Unit.Mob (Mob)
import Game.Unit.Player (Player)
import Game.Unit.Stats
import Game.Unit.Unit
import PreludeUtil (listLens)

-- | All manipulations with units in environment should use this type
data UnitId = MobUnitId Int | PlayerUnitId

-- TODO maybe extract units to a different module?
-- TODO comment
data Environment
  = Environment
      { _player :: Player,
        _mobs :: [(Int, Mob GameEnv)],
        _levels :: [GameLevel],
        _currentLevel :: Int,
        _currentUnitTurn :: Int,
        _modifierFactory :: ModifierFactory,
        _playerEvaluator :: Action -> GameEnv (),
        _mobEvaluators :: [Action -> GameEnv ()]
      }

newtype GameEnv a = GameEnv {unGameEnv :: State Environment a} deriving (Functor, Applicative, Monad, MonadState Environment)

makeLenses ''Environment

runGameEnv :: GameEnv a -> Environment -> (a, Environment)
runGameEnv gameEnv env = runState (unGameEnv gameEnv) env

instance Show Environment where
  show _ = "Environment"

-- | Constructs a new 'Environment'.
makeEnvironment :: Player -> [Mob GameEnv] -> [GameLevel] -> ModifierFactory -> Environment
makeEnvironment player mobs levels factory =
  Environment
    { _player = player,
      _mobs = (zip [0 ..] mobs),
      _levels = levels,
      _currentLevel = 0,
      _currentUnitTurn = 0,
      _modifierFactory = factory,
      _playerEvaluator = const $ return (),
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

affectUnit :: UnitId -> Modifier a -> GameEnv a
affectUnit PlayerUnitId modifier = do
  env <- get
  let (newPlayer, result) = applyModifier modifier (env ^. player)
  modify $ set player newPlayer
  filterDead
  return result
affectUnit (MobUnitId idx) modifier = do
  env <- get
  let (newMob, result) = applyModifier modifier (snd $ (env ^. mobs) !! idx)
  modify $ set (mobs . ix idx . _2) newMob
  filterDead
  return result

unitByCoord :: (Int, Int) -> GameEnv (Maybe UnitId)
unitByCoord coord = do
  units <- getActiveUnits
  filtered <- filterM (\u -> (== coord) <$> affectUnit u Modifier.getPosition) units
  return $ listToMaybe filtered

moveUnit :: UnitId -> (Int, Int) -> GameEnv Bool
moveUnit u pos = do
  lvl <- getCurrentLevel
  let maybeCell = maybeGetCellAt pos lvl
  maybeStats <- affectUnit u Modifier.getStats
  isFree <- isNothing <$> unitByCoord pos
  if isJust $ checkAll maybeCell maybeStats isFree
    then affectUnit u $ Modifier.setCoord pos >> return True
    else return False
  where
    checkAll maybeCell maybeStats isFree = do
      cell <- maybeCell
      stats <- maybeStats
      guard isFree
      return ()

{- unitByCoord :: (Int, Int) -> Environment -> Maybe Unit.AnyUnit
unitByCoord coord env = find ((== coord) . _position . Unit.asUnitData) $ _units env -}

-- | TODO: implement it normally
envAttack :: UnitId -> UnitId -> GameEnv ()
envAttack attackerId attackedId = case (attackerId, attackedId) of
  (PlayerUnitId, PlayerUnitId) -> error "Player attack player"
  (PlayerUnitId, (MobUnitId idx)) -> do
    env <- get
    let fact = env ^. modifierFactory
    let (a, b) = attack fact (env ^. player) (snd $ (env ^. mobs) !! idx)
    setPlayer a >> setMob b idx
  ((MobUnitId idx), PlayerUnitId) -> do
    env <- get
    let fact = env ^. modifierFactory
    let (a, b) = attack fact (snd $ (env ^. mobs) !! idx) (env ^. player)
    setMob a idx >> setPlayer b
  (MobUnitId idx1, MobUnitId idx2) -> do
    env <- get
    let fact = env ^. modifierFactory
    let (a, b) = attack fact (snd $ (env ^. mobs) !! idx1) (snd $ (env ^. mobs) !! idx2)
    setMob a idx1 >> setMob b idx2
  where
    setPlayer :: Player -> GameEnv ()
    setPlayer p = modify $ set player p
    setMob :: Mob GameEnv -> Int -> GameEnv ()
    setMob m idx = modify $ set (mobs . ix idx . _2) m

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
  positions <- traverse (\u -> affectUnit u Modifier.getPosition) units
  portraits <- traverse (\u -> affectUnit u Modifier.getPortrait) units
  return $ foldl (\m ((x, y), p) -> m & ix y . ix x .~ p) terrain (zip positions portraits)
