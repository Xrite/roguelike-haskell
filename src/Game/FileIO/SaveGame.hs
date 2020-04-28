module Game.FileIO.SaveGame
  ( saveGame
  , loadGame
  ) where

import Game.EnvironmentSerialization ()
import Game.Environment
import Data.Binary (encodeFile, decodeFileOrFail)
import Data.Bifunctor (first)
import RIO.FilePath ((</>), (<.>))
import System.Directory (createDirectoryIfMissing)

saveFolder :: FilePath
saveFolder = "saves"

saveExt :: String
saveExt = "rsave"

savePath :: String -> FilePath
savePath name = saveFolder </> name <.> saveExt

-- |Saves game into a save file with provided name.
saveGame :: String -> Environment -> IO ()
saveGame name env = do
  createDirectoryIfMissing True saveFolder
  encodeFile (savePath name) env

-- |Loads game by provided save name or returns an error description.
loadGame :: String -> IO (Either String Environment)
loadGame name = first snd <$> decodeFileOrFail (savePath name)