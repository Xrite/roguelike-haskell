module Game.Unit.UnitSerialization () where

import Data.Binary (Binary)
import Game.ItemSerialization ()
import Game.Position
import Game.Unit.Control (TaggedControl)
import Game.Unit.Inventory (Inventory, WeaponSlots, WearableSlots)
import Game.Unit.Stats (Stats)
import Game.Unit.TimedUnitOps (TimedUnitOps)
import Game.Unit.Unit (LevellingStats, Mob, Player, TaggedMobApplyUnitOp, TaggedPlayerApplyUnitOp, Unit, UnitData)

instance Binary WearableSlots

instance Binary WeaponSlots

instance Binary Inventory

instance Binary TimedUnitOps

instance Binary Stats

instance Binary UnitData

instance Binary TaggedControl

instance Binary LevellingStats

instance Binary Player

instance Binary Mob

instance Binary Unit

instance Binary TaggedPlayerApplyUnitOp

instance Binary TaggedMobApplyUnitOp
