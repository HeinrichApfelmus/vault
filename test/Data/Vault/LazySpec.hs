module Data.Vault.LazySpec (spec) where

import Prelude hiding (lookup)
import Test.Hspec
import Data.Vault.Lazy

spec :: Spec
spec = describe "Data.Vault.LazySpec" $ do

  describe "Locker" $ do

    it "unlock retrieves locked item" $ do
      key <- newKey :: IO (Key Int)
      unlock key (lock key 42) `shouldBe` Just 42

    it "unlock with different key retrievs Nothing" $ do
      key  <- newKey :: IO (Key Int)
      key' <- newKey :: IO (Key Int)
      unlock key' (lock key 42) `shouldBe` Nothing

  describe "Vault" $ do

    it "lookup empty returns Nothing" $ do
      key <- newKey :: IO (Key Int)
      lookup key empty `shouldBe` Nothing
