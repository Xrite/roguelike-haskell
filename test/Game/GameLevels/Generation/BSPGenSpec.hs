module Game.GameLevels.Generation.BSPGenSpec
  ( spec
  ) where

import Control.Lens ((^.))
import Control.Monad.State (runState)
import Control.Monad.State.Lazy (evalState)
import qualified Game.GameLevels.Generation.BSPGen as SUT
import System.Random (mkStdGen)
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "generateLevel does not fail" $ do
    it "room fits tightly" $
      let roomSize = 10
       in generateTightRoom roomSize `shouldSatisfy` isOneTightRoom roomSize
    it "random levels have a few rooms" $
      (fst .
       evalState
         (SUT.generateLevel
            (SUT.GeneratorParameters 20 1.5 10)
            (SUT.Space (SUT.Coord 0 0) (SUT.Coord 100 100))) .
       mkStdGen <$>
       [1 .. 100]) `shouldSatisfy`
      and .
      fmap ((> 4) . length)

generateTightRoom roomSize =
  fst . fst $
  runState
    (SUT.generateLevel
       (SUT.GeneratorParameters roomSize 1 roomSize)
       (SUT.Space (SUT.Coord 0 0) (SUT.Coord (roomSize + 1) (roomSize + 1))))
    (mkStdGen 42)

isOneTightRoom roomSize [room] =
  (room ^. SUT.fromCorner == SUT.Coord 0 0) &&
  (room ^. SUT.toCorner == SUT.Coord (roomSize - 1) (roomSize - 1))
