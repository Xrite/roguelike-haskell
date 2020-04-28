{-# LANGUAGE ScopedTypeVariables #-}

-- | Module responsible for constructing levels from file

module Game.FileIO.FileIO
    ( getSavedLevels
    , getLevelByName
    )
    where

import System.IO
import Game.GameLevels.GameLevel
import Game.GameLevels.MapCell
import Game.GameLevels.MapCellType
import Game.GameLevels.MapCellTypeImpl
import System.Directory
import Control.Exception
import Data.Array.IArray

levelsFolder :: String
levelsFolder = "resources/savedLevels/"

-- | Returns a list of saved levels' names that can be loaded
getSavedLevels :: IO (Either SomeException [String])
getSavedLevels =
  try $ do
    path <- makeAbsolute levelsFolder
    files <- listDirectory path
    return $ map (takeWhile ('.' /=)) files

-- | Loads a GameLevel from a file that corresponds to the given level name
getLevelByName :: String -> IO (Either SomeException GameLevel)
getLevelByName name = do
    path <- makeAbsolute levelsFolder
    readLevelFromFile (path ++ name ++ ".txt")

readLevelFromFile :: FilePath -> IO (Either SomeException GameLevel)
readLevelFromFile filePath =
  try $ do
    fileHandler <- openFile filePath ReadMode
    content <- hGetContents fileHandler
    case readGameLevel content of  
      (Just x) -> return x
      Nothing -> throw (userError "wrong format")

readGameLevel :: String -> Maybe GameLevel
readGameLevel str = do
    _map <- readMap str
    return $ makeGameLevel _map

readMap :: String -> Maybe Map
readMap str = do
    let [(x1, s1)] = reads str :: [(Int, String)]
    let [(y1, s2)] = reads s1 :: [(Int, String)]
    let [(x2, s3)] = reads s2 :: [(Int, String)]
    let [(y2, s4)] = reads s3 :: [(Int, String)]
    _array <- readArray $ tail s4  -- removes '\n'
    return $ makeMap (x1, y1) (x2, y2) _array

readArray :: String -> Maybe (Array (Int, Int) MapCell)
readArray str =
  let lists = map (map readMapCell) (lines str)
   in fmap (listArray ((0, 0), (length lists - 1, length (head lists) - 1))) (sequence $ concat lists)

readMapCell :: Char -> Maybe MapCell
readMapCell str = do 
    mapCellType <- readMapCellType str
    return $ makeEmptyCell mapCellType

readMapCellType :: Char -> Maybe MapCellTypeKey
readMapCellType str = case str of 
    '#' -> Just wallKey
    '+' -> Just hallGroundKey
    '.' -> Just roomGroundKey
    'T' -> Just treeKey
    '%' -> Just bushKey
    '>' -> Just ladderDownKey
    '<' -> Just ladderUpKey
    _ -> Nothing
