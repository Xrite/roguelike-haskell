{-# LANGUAGE RankNTypes #-}

-- | Module responsible for levels generation

module Game.GameLevels.GenerateLevel
  ( testGameLevel
  , randomBSPGeneratedLevel
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
import Game.GameLevels.Generation.GenerationUtil

-- |A small hand-crafted level with various types of cells
testGameLevel :: GameLevel
testGameLevel =
  makeGameLevel $
  makeMap (10, 17) (10, 2) $
  runSTArray $ do
    arrType <- newArray ((0, 0), (20, 20)) roomGroundKey :: ST s (STArray s (Int, Int) MapCellTypeKey)
    writeArray arrType (3, 6) wallKey
    writeArray arrType (10, 2) ladderDownKey
    writeArray arrType (10, 17) ladderUpKey
    foldMap (\ i -> writeArray arrType i bushKey) [(x, 10) | x <- [2..15]]
    mapArray makeEmptyCell arrType

-- |Generates a random level according to given parameters
randomBSPGeneratedLevel :: (RandomGen g) => Space -> GeneratorParameters -> g -> (GameLevel, g)
randomBSPGeneratedLevel s params gen = buildLevel s rooms halls gen'
  where
    ((rooms, halls), gen') = runState (generateLevel params s) gen

buildLevel :: (RandomGen g) => Space -> [Room] -> [Hall] -> g -> (GameLevel, g)
buildLevel s rooms halls gen = (level, gen')
  where
    fromTo start finish = [min start finish .. max start finish]

    ((upLadderPosition, downLadderPosition), gen') = flip runState gen $ liftM2 (,) (pickRandomPlaceInRooms rooms) (pickRandomPlaceInRooms rooms)

    writeInterval :: forall s. STArray s (Int, Int) MapCellTypeKey -> Coord -> Coord -> MapCellTypeKey -> ST s ()
    writeInterval arr (Coord x1 y1) (Coord x2 y2) cellType =
      foldMap (\coord -> writeArray arr coord cellType) [(i, j) | i <- x1 `fromTo` x2, j <- y1 `fromTo` y2]
    levelArray =
      runSTArray $ do
        arrType <-
          newArray (toPair $ s ^. fromCoord, toPair $ s ^. toCoord) wallKey :: ST s (STArray s (Int, Int) MapCellTypeKey)
        foldMap (\hall -> writeInterval arrType (hall ^. startCorner) (hall ^. finishCorner) hallGroundKey) halls
        foldMap (\room -> writeInterval arrType (room ^. fromCorner) (room ^. toCorner) roomGroundKey) rooms
        writeArray arrType upLadderPosition ladderUpKey
        writeArray arrType downLadderPosition ladderDownKey
        mapArray makeEmptyCell arrType
    level = makeGameLevel $ makeMap upLadderPosition downLadderPosition levelArray

pickRandomPlaceInRooms :: (MonadState g m, RandomGen g) => [Room] -> m (Int, Int)
pickRandomPlaceInRooms rooms = do
  room <- (rooms !!) <$> mRandomR (0, length rooms - 1)
  toPair <$> mRandomR (room ^. fromCorner, room ^. toCorner)