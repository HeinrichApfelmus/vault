{-----------------------------------------------------------------------------
    Vault
    
    A typed, persistent store for values of arbitrary types
    
    This implementation uses  unsafeCoerce  for reasons of efficiency.
    See  http://apfelmus.nfshost.com/blog/2011/09/04-vault.html
    for an implementation that doesn't need to bypass the type checker.
------------------------------------------------------------------------------}
module Data.Vault (
    Vault, Key,
    empty, newKey, lookup, insert, delete,
    ) where

import Prelude hiding (lookup)
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Unique

import GHC.Base (Any)   -- ghc specific tricks
import Unsafe.Coerce (unsafeCoerce)

-- | A typed, persistent store for values of arbitrary types.
newtype Vault = Vault (Map Unique Any)
-- | Keys for the vault
newtype Key a  = Key Unique

-- | The empty vault.
empty :: Vault
empty = Vault Map.empty

-- | Create a new key for use with a vault.
newKey :: IO (Key a)
newKey = Key <$> newUnique

-- | Lookup the value of a key in the vault.
lookup :: Key a -> Vault -> Maybe a
lookup (Key k) (Vault m) = unsafeCoerce <$> Map.lookup k m 

-- | Insert a value for a given key. Overwrites any previous value.
insert :: Key a -> a -> Vault -> Vault
insert (Key k) x (Vault m) = Vault $ Map.insert k (unsafeCoerce x) m

-- | Delete a key from the vault.
delete :: Key a -> Vault -> Vault
delete (Key k) (Vault m) = Vault $ Map.delete k m

