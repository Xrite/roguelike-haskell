module UI.Keys where

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