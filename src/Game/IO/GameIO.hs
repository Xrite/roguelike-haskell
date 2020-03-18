{-# LANGUAGE MultiParamTypeClasses #-}
module Game.IO.GameIO where

-- I'll use it until actual io becomes available (Vlad)
-- 


newtype GameIO a = Maybe a

pureGameIO :: a -> GameIO a
pureGameIO = undefined
