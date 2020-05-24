module Game.Unit.UnitSerialization () where

import Data.Binary (Binary)
import Game.Unit.Stats (Stats)
import Game.Unit.TimedUnitOps (TimedUnitOps)
import Game.Unit.Inventory (Inventory, WearableSlots, WeaponSlots)
import Game.ItemSerialization ()
import Game.Unit.Unit (LevellingStats, Player, Mob, Unit, UnitData)
import Game.Unit.Control (TaggedControl)

instance Binary WearableSlots
instance Binary WeaponSlots
instance Binary Inventory

instance Binary TimedUnitOps

instance Binary Stats
instance Binary (UnitData pos)

instance Binary TaggedControl

instance Binary LevellingStats
instance Binary (Player pos)
instance Binary (Mob pos)
instance Binary (Unit pos)