module Game.Server.Server where

import Control.Lens
import Control.Monad.Free
import Control.Monad.State
import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import Data.Sequence ((<|), (|>))
import Game.Environment
import Game.Unit

type SessionId = Int

data ServerData
  = ServerData
      { _sessions :: IntMap.IntMap Session
      }

data Session
  = Session
      { _env :: Environment
      }

emptyServer :: ServerData
emptyServer =
  ServerData
    { _sessions = IntMap.empty
    }

newtype Server a = Server {runServer :: StateT ServerData IO a}

makeSession env = Session {_env = env}

-- | Create new session
createNewSession :: Server SessionId
createNewSession = undefined

{- createNewSession = do
    seed <- liftIO $ getStdRandom random
    let lvl = fst $ randomBSPGeneratedLevel (GU.Space (GU.Coord 0 0) (GU.Coord 50 50)) (GeneratorParameters 10 1.7 5) $ mkStdGen seed
    let env = makeEnvironment [] [] [lvl]
    let newSession = makeSession env
    sid <- Seq.length <$> gets (sessions)
    modify $ over sessions (<| newSession)
    return sid -}

-- | Add new player to the session
addNewPlayerToSession :: SessionId -> Server PlayerId
addNewPlayerToSession sid = undefined

--    where
--         ourPlayer = makeSomePlayer $ makeUnitData 0 startCoord 'λ'

-- | Remove player from session if this player is present in the session
removePlayerFromSession :: SessionId -> PlayerId -> Server ()
removePlayerFromSession = undefined

-- | Make action
playerMakeAction :: SessionId -> PlayerId -> Action -> Server ()
playerMakeAction = undefined
 
-- | Click slot
playerClickSlot :: SessionId -> PlayerId -> Int -> Server ()
playerClickSlot = undefined

-- | Click item
playerClickItem :: SessionId -> PlayerId -> Int -> Server ()
playerClickItem = undefined
{- makeSomePlayer :: UnitData -> Player
makeSomePlayer = makeDefaultPlayer . (stats . health %~ (*2))

makeUnitData :: Int -> (Int, Int) -> Char -> UnitData
makeUnitData level position render =
  createUnitData
    (uncheckedPosition level position)
    (Stats.Stats 10 10 10 1)
    Game.Unit.TimedUnitOps.empty
    someInventory
    (createWeapon "drugged fist" (effectAtom (damage 1) >> effectTypical "confuse") 'A')
    render
  where
    someInventory =
        addItem (weaponToItem $ createWeapon "saber" (effectAtom (damage 5)) '?') $
        addItem (wearableToItem $ createWearable "pointy hat" Head (effectAtom (heal 10)) (return ()) '^') $
        addItem (wearableToItem $ createWearable "uncomfortable shoes" Legs (effectAtom confuse) (return ()) '"')
        emptyInventory
 -}
