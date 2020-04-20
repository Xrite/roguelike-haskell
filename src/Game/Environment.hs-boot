module Game.Environment
  ( Environment,
    UnitId,
    GameEnv,
    getCurrentLevel,
    unitByCoord,
    envAttack,
    playerId,
    affectUnit,
    moveUnit,
    runGameEnv,
    renderEnvironment,
    evalAction,
    randomRGameEnv
  )
where

import Control.Monad.State
import Game.GameLevels.GameLevel
import Game.Modifiers.UnitOp
import Game.Modifiers.UnitOpFactory
import Game.Unit.Stats
import Game.Unit.Action
import System.Random

-- | All manipulations with units in environment should use this type
data UnitId = MobUnitId Int | PlayerUnitId

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

affectUnit :: UnitId -> UnitOp a -> GameEnv a

unitByCoord :: (Int, Int) -> GameEnv (Maybe UnitId)

moveUnit :: UnitId -> (Int, Int) -> GameEnv Bool

envAttack :: UnitId -> UnitId -> GameEnv ()

evalAction :: UnitId -> Action -> GameEnv ()

getCurrentLevel :: GameEnv GameLevel

playerId :: Environment -> UnitId

renderEnvironment :: GameEnv [String]