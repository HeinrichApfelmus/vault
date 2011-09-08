{-----------------------------------------------------------------------------
    Vault
    
    A typed, persistent store for values of arbitrary types
    
    This implementation uses  unsafeCoerce  for reasons of efficiency.
    See  http://apfelmus.nfshost.com/blog/2011/09/04-vault.html
    for an implementation that doesn't need to bypass the type checker.
------------------------------------------------------------------------------}
module Data.Vault.ST (
    Vault, Key,
    empty, newKey, lookup, insert, adjust, delete, union,
    ) where

import Prelude hiding (lookup)
import Data.Monoid hiding (Any)
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Unique
import Control.Monad.ST

import GHC.Exts (Any)   -- ghc specific tricks
import Unsafe.Coerce (unsafeCoerce)

-- | A typed, persistent store for values of arbitrary types.
newtype Vault s = Vault (Map Unique Any)
-- | Keys for the vault
newtype Key s a = Key Unique

instance Monoid (Vault s) where
    mempty = empty
    mappend = union

-- | The empty vault.
empty :: Vault s
empty = Vault Map.empty

-- | Create a new key for use with a vault.
newKey :: ST s (Key s a)
newKey = Key <$> unsafeIOToST newUnique

-- | Lookup the value of a key in the vault.
lookup :: Key s a -> Vault s -> Maybe a
lookup (Key k) (Vault m) = unsafeCoerce <$> Map.lookup k m 

-- | Insert a value for a given key. Overwrites any previous value.
insert :: Key s a -> a -> Vault s -> Vault s
insert (Key k) x (Vault m) = Vault $ Map.insert k (unsafeCoerce x) m

-- | Adjust the value for a given key if it's present in the vault.
adjust :: (a -> a) -> Key s a -> Vault s -> Vault s
adjust f (Key k) (Vault m) = Vault $ Map.alter f' k m
    where f' = unsafeCoerce . f . unsafeCoerce

-- | Delete a key from the vault.
delete :: Key s a -> Vault s -> Vault s
delete (Key k) (Vault m) = Vault $ Map.delete k m

-- | Merge two vaults (left-biased).
union :: Vault s -> Vault s -> Vault s
union (Vault m) (Vault m') = Vault $ Map.union m m'
