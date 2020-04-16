{-# LANGUAGE RankNTypes #-}

module PreludeUtil
  ( setAt
  , listLens
  ) where

import Control.Lens

setAt :: Int -> a -> [a] -> [a]
setAt n _ _  | n < 0 = error "negative index in setAt"
setAt n _ [] = error "too big index in setAt"
setAt 0 v (_ : xs) = v : xs
setAt n v (x : xs) = x : setAt (n - 1) v xs

listLens :: Int -> Lens' [a] a
listLens idx = lens (!! idx) (flip $ setAt idx)
