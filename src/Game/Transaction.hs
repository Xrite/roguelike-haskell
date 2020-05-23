module Transaction where

import Game.Unit (Action)

data Transaction = [TransactionAtom]

data TransactionAtom
    = UnitAction Action