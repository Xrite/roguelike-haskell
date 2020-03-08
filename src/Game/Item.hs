module Game.Item where

import Game.Effect

data WearableType = Head | Chest | Legs

data Item = Consumable { effect :: Effect () } 