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


-- TODO make not show not read

-- hPrint          :: Show a => Handle -> a -> IO ()
-- hPrint hdl      =  hPutStrLn hdl . show

getSavedLevels :: Exception e => IO (Either e [String])
getSavedLevels = try $ do
	files <- getDirectoryContents "/resources/savedLevels" 
	return $ map (takeWhile ((/=) '.'))files

getLevelByName :: Exception e => String -> IO (Either e GameLevel)
getLevelByName name = readLevelFromFile (name ++ ".txt")

saveLevelToFile :: GameLevel -> FilePath -> IO ()
saveLevelToFile gameLevel filePath = do
	fileHandler <- openFile filePath WriteMode
	hPrint fileHandler gameLevel
	hClose fileHandler

readLevelFromFile :: Exception e => FilePath -> IO (Either e GameLevel)
readLevelFromFile filePath = try $ do
	fileHandler <- openFile filePath ReadMode
	content <- hGetContents fileHandler
	hClose fileHandler
	return $ read content

instance Show GameLevel where
	show (GameLevel map) = show map

instance Read GameLevel where
	readsPrec _ str = [(makeGameLevel $ read str,"")]

instance Show Map where
	show (Map cells) = show cells

instance Read Map where
	readsPrec _ str = [(makeMap $ read str, "")]

instance Show MapCell where
	show (MapCell _cellType _) = show _cellType

instance Read MapCell where
	readsPrec _ str = [(makeEmptyCell $ read str, "")] 

instance Show MapCellType where
    show (MapCellType _cellRender _ _ _ _) = show _cellRender

instance Read MapCellType where
    readsPrec _ str = case str of 
	    "#" -> [(wall, "")]
	    "+" -> [(hallGround, "")]
	    "." -> [(roomGround, "")]
	    "T" -> [(tree, "")]
	    "%" -> [(bush, "")]
	    ">" -> [(ladderDown, "")]
	    "<" -> [(ladderUp, "")]
	    _ -> []