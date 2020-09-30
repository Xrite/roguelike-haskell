module UI.Keys where

data Arrows = Up
            | Down
            | Left
            | Right
            | UpRight
            | UpLeft
            | DownRight
            | DownLeft
            | Center

data Keys = Letter Char
          | Enter
          | Space
