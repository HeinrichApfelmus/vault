import Prelude hiding (lookup)

import Control.Monad.ST (RealWorld, stToIO)
import Test.Hspec
import MODULE_UNDER_TEST

newKey' :: IO (Key RealWorld a)
newKey' = stToIO newKey

spec :: Spec
spec = describe TEST_NAME $ do

  describe "Locker" $ do

    it "unlock retrieves locked item" $ do
      key <- newKey' :: IO (Key RealWorld Int)
      unlock key (lock key 42) `shouldBe` Just 42

    it "unlock with different key retrieves Nothing" $ do
      key  <- newKey' :: IO (Key RealWorld Int)
      key' <- newKey' :: IO (Key RealWorld Int)
      unlock key' (lock key 42) `shouldBe` Nothing

  describe "Vault" $ do

    it "lookup empty returns Nothing" $ do
      key <- newKey' :: IO (Key RealWorld Int)
      lookup key empty `shouldBe` Nothing
