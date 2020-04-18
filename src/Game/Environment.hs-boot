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

import Control.Monad.State
import Game.GameLevels.GameLevel
import Game.Modifiers.Modifier as Modifier
import Game.Modifiers.ModifierFactory
import Game.Unit.Mob (Mob)
import Game.Unit.Player (Player)
import Game.Unit.Stats
import Game.Unit.Unit

-- | All manipulations with units in environment should use this type
data UnitId = MobUnitId Int | PlayerUnitId

-- TODO maybe extract units to a different module?
-- TODO comment
data Environment

newtype GameEnv a = GameEnv {unGameEnv :: State Environment a}

instance Functor GameEnv
instance Applicative GameEnv
instance Monad GameEnv

runGameEnv :: GameEnv a -> Environment -> (a, Environment)

makeEnvironment :: Player -> [Mob GameEnv] -> [GameLevel] -> ModifierFactory -> Environment

filterDead :: GameEnv ()

getActiveUnits :: GameEnv [UnitId]

affectUnit :: UnitId -> Modifier a -> GameEnv a

unitByCoord :: (Int, Int) -> GameEnv (Maybe UnitId)

moveUnit :: UnitId -> (Int, Int) -> GameEnv Bool

envAttack :: UnitId -> UnitId -> GameEnv ()

evalAction :: UnitId -> Action -> GameEnv ()

getCurrentLevel :: GameEnv GameLevel

playerId :: Environment -> UnitId

renderEnvironment :: GameEnv [String]