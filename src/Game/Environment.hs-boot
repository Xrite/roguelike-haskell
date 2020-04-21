module Game.Environment
  ( Environment,
    UnitId,
    GameEnv,
    FailableGameEnv,
    UnitIdError(..),
    getCurrentLevel,
    unitByCoord,
    envAttack,
    playerId,
    affectUnit,
    moveUnit,
    runGameEnv,
    renderEnvironment,
    evalAction,
    randomRGameEnv,
    getAction,
  )
where

import Control.Monad.State
import Game.GameLevels.GameLevel
import Game.Modifiers.UnitOp
import Game.Modifiers.UnitOpFactory
import Game.Unit.Stats
import Game.Unit.Action
import System.Random
import Control.Monad.Except

-- | All manipulations with units in environment should use this type
data UnitId = MobUnitId Int | PlayerUnitId

data UnitIdError

type FailableGameEnv err = ExceptT err GameEnv

instance Eq UnitId

-- TODO maybe extract units to a different module?
-- TODO comment
data Environment

newtype GameEnv a = GameEnv {unGameEnv :: State Environment a}

instance Functor GameEnv
instance Applicative GameEnv
instance Monad GameEnv

randomRGameEnv :: Random a => (a, a) -> GameEnv a

runGameEnv :: GameEnv a -> Environment -> (a, Environment)


filterDead :: GameEnv ()

getActiveUnits :: GameEnv [UnitId]

affectUnit :: UnitId -> UnitOp a -> FailableGameEnv UnitIdError a

unitByCoord :: (Int, Int) -> GameEnv (Maybe UnitId)

moveUnit :: UnitId -> (Int, Int) -> FailableGameEnv UnitIdError Bool

envAttack :: UnitId -> UnitId -> FailableGameEnv UnitIdError ()

evalAction :: UnitId -> Action -> FailableGameEnv UnitIdError Bool

getAction :: UnitId -> FailableGameEnv UnitIdError Action

getCurrentLevel :: GameEnv GameLevel

playerId :: Environment -> UnitId

renderEnvironment :: GameEnv [String]