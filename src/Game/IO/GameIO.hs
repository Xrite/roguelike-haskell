{-# LANGUAGE MultiParamTypeClasses #-}
module Game.IO.GameIO where

-- I'll use it until actual io becomes available (Vlad)
-- 

import           UI                             ( UI )

newtype GameIO a = Maybe a

pureGameIO :: a -> GameIO a
pureGameIO = undefined
