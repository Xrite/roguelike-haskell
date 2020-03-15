{-# LANGUAGE RankNTypes #-}
module Game.GameLevels.GenerateLevel
  ( testGameLevel
  , randomLevel
  ) where

import Game.GameLevels.MapCellTypeImpl
import Game.GameLevels.MapCellType
import Game.GameLevels.GameLevel
import Game.GameLevels.MapCell
import Data.Array.ST
import Control.Monad.ST
import Game.GameLevels.Generation.BSPGen
import Game.GameLevels.Generation.RandomMonad
import System.Random
import Control.Monad.State
import Control.Lens ((^.))

testGameLevel :: GameLevel
testGameLevel =
  makeGameLevel $
  makeMap $
  runSTArray $ do
    arrType <- newArray ((0, 0), (20, 20)) roomGround :: ST s (STArray s (Int, Int) MapCellType)
    writeArray arrType (3, 6) wall
    writeArray arrType (10, 2) ladderDown
    writeArray arrType (10, 17) ladderUp
    foldMap (\ i -> writeArray arrType i bush) [(x, 10) | x <- [2..15]]
    mapArray makeEmptyCell arrType

randomLevel :: (RandomGen g) => Space -> GeneratorParameters -> g -> (GameLevel, g)
randomLevel s params gen = (level, gen')
  where
    fromTo start finish = [min start finish .. max start finish]

    ((rooms, halls), gen') = runState (generateLevel params s) gen

    writeInterval :: forall s. STArray s (Int, Int) MapCellType -> Coord -> Coord -> MapCellType -> ST s ()
    writeInterval arr (Coord x1 y1) (Coord x2 y2) cellType =
      foldMap (\coord -> writeArray arr coord cellType) [(i, j) | i <- x1 `fromTo` x2, j <- y1 `fromTo` y2]

    levelArray =
      runSTArray $ do
        arrType <-
          newArray (toPair $ s ^. fromCoord, toPair $ s ^. toCoord) wall :: ST s (STArray s (Int, Int) MapCellType)
        foldMap (\hall -> writeInterval arrType (hall ^. startCorner) (hall ^. finishCorner) hallGround) halls
        foldMap (\room -> writeInterval arrType (room ^. fromCorner) (room ^. toCorner) roomGround) rooms
        mapArray makeEmptyCell arrType
    level = makeGameLevel $ makeMap levelArray
