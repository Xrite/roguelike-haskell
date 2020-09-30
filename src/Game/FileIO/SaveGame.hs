module Game.FileIO.SaveGame
  ( saveGame
  , loadGame
  ,	removeGame
  ) where

import Game.EnvironmentSerialization ()
import Game.Environment (EnvMemento)
import Data.Binary (encodeFile, decodeFileOrFail)
import Data.Bifunctor (first)
import RIO.FilePath ((</>), (<.>))
import System.Directory (createDirectoryIfMissing, removeFile, doesFileExist)

saveFolder :: FilePath
saveFolder = "saves"

saveExt :: String
saveExt = "rsave"

savePath :: String -> FilePath
savePath name = saveFolder </> name <.> saveExt

-- |Saves game into a save file with provided name.
saveGame :: String -> EnvMemento -> IO ()
saveGame name env = do
  createDirectoryIfMissing True saveFolder
  encodeFile (savePath name) env

-- |Removes the file of the game with the provided name.
removeGame :: String -> IO ()
removeGame name = let filePath = savePath name in do
	fileExist <- doesFileExist filePath
	if fileExist 
		then removeFile $ savePath name
		else return ()

-- |Loads game by provided save name or returns an error description.
loadGame :: String -> IO (Either String EnvMemento)
loadGame name = let filePath = savePath name in do 
	fileExist <- doesFileExist filePath
	if fileExist 
		then first snd <$> decodeFileOrFail filePath 
		else return $ Left "autosave does not exist"