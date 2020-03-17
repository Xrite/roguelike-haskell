{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Game.GameLevels.Generation.RandomMonad where

import Control.Monad.State.Lazy
import System.Random

-- | 'RandomMonad' provides an interface for random values generation without
-- need to explicitly store every version of a generator.
--
-- Minimal complete definition: 'generate'

class (Monad m, RandomGen g) => RandomMonad g m | m -> g where
    -- |Using provided function and a monad's generator produces a random value. 
    generate :: (g -> (a, g)) -> m a

    -- |Uses 'Random' generation implementation to generate a value in a provided interval.
    mRandomR :: Random a => (a, a) -> m a
    mRandomR interval = generate $ randomR interval

-- |Using random generator in the state of 'MonadState' implements 'RandomMonad'.
--
-- This instance has a weird type that requires in "UndecidableInstances" 
instance (RandomGen g, Monad m, MonadState g m) => RandomMonad g m where
  generate gen = do
      g <- get
      let (val, g') = gen g
      put g'
      return val
