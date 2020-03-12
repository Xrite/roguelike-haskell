module Game.GameLevels.Generation.RandomMonad
    ( stRandomR
    , generate
    )
where

import Control.Monad.State.Lazy
import System.Random

stRandomR :: (MonadState s m, Random a, RandomGen s) => (a, a) -> m a
stRandomR interval = generate $ randomR interval

generate :: (MonadState g m) => (g -> (a, g)) -> m a
generate gen = do
    g <- get
    let (val, g') = gen g
    put g'
    return val