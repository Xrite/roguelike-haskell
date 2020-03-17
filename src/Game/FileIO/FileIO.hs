{-# LANGUAGE ScopedTypeVariables #-}

-- Module responsible for reading levels from file

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

levelsFolder :: [Char]
levelsFolder = "resources/savedLevels/"

-- Returns a list of saved levels name that could be loaded
getSavedLevels :: IO (Either SomeException [String])
getSavedLevels = try $ do
    path <- makeAbsolute levelsFolder
    files <- listDirectory path
    return $ map (takeWhile ((/=) '.'))files

-- Loads a GameLevel from a file that corresponds to the given level name
getLevelByName :: String -> IO (Either SomeException GameLevel)
getLevelByName name = do
    path <- makeAbsolute levelsFolder
    readLevelFromFile (path ++ name ++ ".txt")

readLevelFromFile :: FilePath -> IO (Either SomeException GameLevel)
readLevelFromFile filePath = try $ do
    fileHandler <- openFile filePath ReadMode
    content <- hGetContents fileHandler
    case (readGameLevel content) of
        (Just x) -> return x
        Nothing -> throw (userError "wrong format")

readGameLevel :: String -> Maybe GameLevel
readGameLevel str = do 
    _map <- readMap str
    return $ makeGameLevel _map

readMap :: String -> Maybe Map
readMap str = do
    _array <- readArray str
    return $ makeMap _array

readArray :: String -> Maybe (Array (Int, Int) MapCell)
readArray str = let lists = map (map (readMapCell)) (lines str) in
    fmap (listArray ((0, 0), (length lists, (length (lists !! 0))))) (sequence $ concat lists)

readMapCell :: Char -> Maybe MapCell
readMapCell str = do 
    mapCellType <- readMapCellType str
    return $ makeEmptyCell mapCellType

readMapCellType :: Char -> Maybe MapCellType
readMapCellType str = case str of 
    '#' -> Just wall
    '+' -> Just hallGround
    '.' -> Just roomGround
    'T' -> Just tree
    '%' -> Just bush
    '>' -> Just ladderDown
    '<' -> Just ladderUp
    _ -> Nothing
