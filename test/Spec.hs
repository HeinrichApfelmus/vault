module Main (main) where
 
import Test.Hspec
import qualified Data.Vault.ST.LazySpec
import qualified Internal.Data.Vault.ST.IORefSpec
 
main :: IO ()
main = hspec $ do
  Data.Vault.ST.LazySpec.spec
  Internal.Data.Vault.ST.IORefSpec.spec
