{-# LANGUAGE ScopedTypeVariables #-}

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

levelsFolder = "resources/savedLevels/"
-- TODO make not show not read
-- TODO handle errors

-- hPrint          :: Show a => Handle -> a -> IO ()
-- hPrint hdl      =  hPutStrLn hdl . show

getSavedLevels :: Exception e => IO (Either e [String])
getSavedLevels = try $ do
    path <- makeAbsolute levelsFolder
    files <- listDirectory path
    return $ map (takeWhile ((/=) '.'))files

getLevelByName :: Exception e => String -> IO (Either e GameLevel)
getLevelByName name = do
    path <- makeAbsolute levelsFolder
    readLevelFromFile (path ++ name ++ ".txt")

saveLevelToFile :: GameLevel -> FilePath -> IO ()
saveLevelToFile gameLevel filePath = do
	fileHandler <- openFile filePath WriteMode
	hPrint fileHandler gameLevel
	hClose fileHandler

readLevelFromFile :: Exception e => FilePath -> IO (Either e GameLevel)
readLevelFromFile filePath = try $ do
    fileHandler <- openFile filePath ReadMode
    content <- hGetContents fileHandler
    return $ readGameLevel content

instance Show GameLevel where
	show (GameLevel map) = show map

readGameLevel :: String -> GameLevel
readGameLevel str = makeGameLevel $ readMap str

instance Show Map where
	show (Map cells) = show cells

readMap :: String -> Map
readMap str = makeMap $ readArray str

readArray :: String -> Array (Int, Int) MapCell
readArray str = let lists = map (map readMapCell) (lines str) in
    listArray ((0, 0), (length lists, (length (lists !! 0)))) (concat lists)

instance Show MapCell where
	show (MapCell _cellType _) = show _cellType

readMapCell :: Char -> MapCell
readMapCell str = makeEmptyCell $ readMapCellType str

instance Show MapCellType where
    show (MapCellType _cellRender _ _ _ _) = show _cellRender

readMapCellType :: Char -> MapCellType
readMapCellType str = case str of 
    '#' -> wall
    '+' -> hallGround
    '.' -> roomGround
    'T' -> tree
    '%' -> bush
    '>' -> ladderDown
    '<' -> ladderUp
