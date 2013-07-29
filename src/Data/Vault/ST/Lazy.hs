{-----------------------------------------------------------------------------
    vault
------------------------------------------------------------------------------}
{-# LANGUAGE CPP #-}
module Data.Vault.ST.Lazy (
    -- * Synopsis
    -- | A persistent store for values of arbitrary types.
    -- Variant for the 'ST' monad.
    --
    -- The 'Vault' type in this module is strict in the keys but lazy in the values.

    
    -- * Vault
    Vault, Key,
    empty, newKey, lookup, insert, adjust, delete, union,
    
    -- * Locker
    Locker,
    lock, unlock,
    ) where

import Data.Monoid (Monoid(..))
import Prelude hiding (lookup)
import Control.Monad.ST

{-
    The GHC-specific implementation uses  unsafeCoerce 
    for reasons of efficiency.
    
    See  http://apfelmus.nfshost.com/blog/2011/09/04-vault.html
    for the second implementation that doesn't need to
    bypass the type checker.
-}
#if UseGHC
import qualified Data.Vault.ST.GHC_Lazy as ST
#else
import qualified Data.Vault.ST.Pure_Lazy as ST
#endif

{-----------------------------------------------------------------------------
    Vault
------------------------------------------------------------------------------}
-- | A persistent store for values of arbitrary types.
-- 
-- This variant is the simplest and creates keys in the 'IO' monad.
-- See the module "Data.Vault.ST" if you want to use it with the 'ST' monad instead.
--
-- > type Vault :: * -> *
-- > instance Monoid Vault
type Vault = ST.Vault

instance Monoid (ST.Vault s) where
    mempty = empty
    mappend = union

-- | Keys for the vault.
--
-- > type Key :: * -> * -> *
type Key = ST.Key

-- | The empty vault.
empty :: Vault s
empty = ST.empty

-- | Create a new key for use with a vault.
newKey :: ST s (Key s a)
newKey = ST.newKey

-- | Lookup the value of a key in the vault.
lookup :: Key s a -> Vault s -> Maybe a
lookup = ST.lookup

-- | Insert a value for a given key. Overwrites any previous value.
insert :: Key s a -> a -> Vault s -> Vault s
insert = ST.insert

-- | Adjust the value for a given key if it's present in the vault.
adjust :: (a -> a) -> Key s a -> Vault s -> Vault s
adjust = ST.adjust

-- | Delete a key from the vault.
delete :: Key s a -> Vault s -> Vault s
delete = ST.delete

-- | Merge two vaults (left-biased).
union :: Vault s -> Vault s -> Vault s
union = ST.union

{-----------------------------------------------------------------------------
    Locker
------------------------------------------------------------------------------}
-- | A persistent store for a single value.
--
-- > type Locker :: * -> *
type Locker = ST.Locker

-- | Put a single value into a 'Locker'.
lock :: Key s a -> a -> Locker s
lock = ST.lock

-- | Retrieve the value from the 'Locker'.
unlock :: Key s a -> Locker s -> Maybe a
unlock = ST.unlock
