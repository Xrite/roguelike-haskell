{-# LANGUAGE NoImplicitPrelude #-}
-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util
  ( plus2
  , setAt
  ) where

import RIO

plus2 :: Int -> Int
plus2 = (+ 2)

setAt :: Int -> a -> [a] -> [a]
setAt n _ _  | n < 0 = error "negative index in setAt"
setAt n _ [] = error "too big index in setAt"
setAt 0 v (_ : xs) = v : xs
setAt n v (x : xs) = x : setAt (n - 1) v xs
