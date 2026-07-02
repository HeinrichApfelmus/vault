import Prelude hiding (lookup)

import Control.Concurrent (forkFinally, threadDelay)
import Control.Concurrent.MVar (MVar, putMVar, takeMVar, newEmptyMVar)
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

    -- Even though the code contained a potential race condition,
    -- this test was unable to trigger it.
    -- Still, we keep it as a regression test.
    it "no race condition for simultaneous unlock" $ do
      key  <- newKey' :: IO (Key RealWorld Int)
      let check n = unlock key (lock key n) `shouldBe` Just n
      race $ map check [1..500]

  describe "Vault" $ do

    it "lookup empty returns Nothing" $ do
      key <- newKey' :: IO (Key RealWorld Int)
      lookup key empty `shouldBe` Nothing

{-----------------------------------------------------------------------------
    Helper functions
------------------------------------------------------------------------------}
-- | Race multiple IO actions against each other.
race :: [IO ()] -> IO ()
race xs = do
    mvars <- mapM (\_ -> newEmptyMVar :: IO (MVar ())) xs
    sequence $
        zipWith
          (\mvar m -> forkFinally (threadDelay 10 >> m) (const $ putMVar mvar ()))
          mvars
          xs
    mapM_ takeMVar mvars
