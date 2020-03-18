{-# LANGUAGE NoImplicitPrelude #-}
module UI.Keys where

import Data.Maybe (Maybe(..))

import Data.Char (Char)
import Data.List (lookup)

data Arrows = Up
            | Down
            | Left
            | Right
            | UpRight
            | UpLeft
            | DownRight
            | DownLeft

data Keys = Letter Char
          | Enter
          | Space

parseArrows :: Keys -> Maybe Arrows
parseArrows (Letter c) = lookup c keyArrows
  where
    keyArrows = [('j', Up),
                 ('k', Down),
                 ('h', Left),
                 ('l', Right),
                 ('u', UpRight),
                 ('y', UpLeft),
                 ('b', DownRight),
                 ('n', DownLeft)]
parseArrows _ = Nothing