{-# LANGUAGE NoImplicitPrelude #-}
module FileIOSpec (spec) where

import Import
import Game.FileIO.FileIO
import Game.GameLevels.GameLevel
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "fileIO" $ do
    it "checkSavedLevels" $ checkSavedLevelsList
    it "checkGetLevelByName" $ checkGetLevelByName "Level2"
    it "throwGetLevelByWrongName" $ (checkGetLevelByName "abracadabra") `shouldThrow` anyException

checkSavedLevelsList :: IO ()
checkSavedLevelsList = do 
    Right result <- getSavedLevels :: IO (Either SomeException [FilePath])
    result `shouldBe` ["Level1", "Level2", "Level3"]

checkGetLevelByName :: String -> IO ()
checkGetLevelByName name = do
    Right (GameLevel _map) <- getLevelByName name :: IO (Either SomeException GameLevel)
    (getMapSize _map) `shouldBe` ((0, 0), (15, 18))
