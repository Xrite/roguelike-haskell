module Game.Item where

import Game.Effect

data WearableType = Head | Chest | Legs

data Item = Consumable { effect :: Effect () }

-- anton make this plz
itemRender :: Item -> Char
itemRender = undefined 