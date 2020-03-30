{-# LANGUAGE RankNTypes #-}

-- | Module responsible for levels generation

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

-- |A small hand-crafted level with various types of cells
testGameLevel :: GameLevel
testGameLevel =
  makeGameLevel $
  makeMap (10, 17) (10, 2) $
  runSTArray $ do
    arrType <- newArray ((0, 0), (20, 20)) roomGround :: ST s (STArray s (Int, Int) MapCellType)
    writeArray arrType (3, 6) wall
    writeArray arrType (10, 2) ladderDown
    writeArray arrType (10, 17) ladderUp
    foldMap (\ i -> writeArray arrType i bush) [(x, 10) | x <- [2..15]]
    mapArray makeEmptyCell arrType

-- |Generates a random level according to given parameters
randomLevel :: (RandomGen g) => Space -> GeneratorParameters -> g -> (GameLevel, g)
randomLevel s params gen = (level, gen'')
  where
    fromTo start finish = [min start finish .. max start finish]
    ((rooms, halls), gen') = runState (generateLevel params s) gen
    
    ((upLadderPosition, downLadderPosition), gen'') = flip runState gen' $ liftM2 (,) (pickRandomPlaceInRooms rooms) (pickRandomPlaceInRooms rooms)
    
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
    level = makeGameLevel $ makeMap upLadderPosition downLadderPosition levelArray

 
pickRandomPlaceInRooms :: (MonadState g m, RandomGen g) => [Room] -> m (Int, Int)
pickRandomPlaceInRooms rooms = do
  room <- (rooms !!) <$> mRandomR (0, length rooms - 1)
  toPair <$> mRandomR (room ^. fromCorner, room ^. toCorner)