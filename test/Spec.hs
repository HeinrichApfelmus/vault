module Main (main) where
 
import Test.Hspec
import qualified Data.Vault.LazySpec
 
main :: IO ()
main = hspec $ do
  Data.Vault.LazySpec.spec
 