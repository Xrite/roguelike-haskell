module Game.IO.GameIO where

-- I'll use it until actual io becomes available (Vlad)
-- 

import           UI                             ( UI )

newtype GameIO a = Maybe a

pureGameIO :: a -> GameIO a
pureGameIO = undefined

class (Monad m) => GameUI m where
    showUI :: UI (m a) -> m a
