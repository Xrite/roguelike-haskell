{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Game.GameLevels.Generation.RandomMonad where

import Control.Monad.State.Lazy
import System.Random

class (Monad m, RandomGen g) => RandomMonad g m | m -> g where
    generate :: (g -> (a, g)) -> m a

    mRandomR :: Random a => (a, a) -> m a
    mRandomR interval = generate $ randomR interval

instance (RandomGen g, Monad m, MonadState g m) => RandomMonad g m where
  generate gen = do
      g <- get
      let (val, g') = gen g
      put g'
      return val
