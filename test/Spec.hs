module Main (main) where
 
import Test.Hspec
import qualified Data.Vault.LazySpec
import qualified Internal.Data.Vault.IORefSpec
 
main :: IO ()
main = hspec $ do
  Data.Vault.LazySpec.spec
  Internal.Data.Vault.IORefSpec.spec
