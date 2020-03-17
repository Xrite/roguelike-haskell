module Game.GameLevels.Generation.BSPGenSpec
  ( spec
  ) where

import Control.Monad.State (runState)
import qualified Game.GameLevels.Generation.BSPGen as SUT
import System.Random (mkStdGen)
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "generateLevel does not fail" $ do
    it "room fits tightly" $
      runState
        (SUT.generateLevel (SUT.GeneratorParameters 10 1 10) (SUT.Space (SUT.Coord 0 0) (SUT.Coord 10 10)))
        (mkStdGen 42) `shouldSatisfy`
      ((> 0) . length)
     
