#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE RoleAnnotations #-}
#endif

module Data.Vault.ST.LAZINESS (
    -- * Vault
    Vault, Key,
    empty, newKey, lookup, insert, adjust, delete, union,

    -- * Locker
    Locker,
    lock, unlock,
    ) where

import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup((<>)))
import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Control.Monad.ST
import Control.Monad.ST.Unsafe as STUnsafe

import Data.Unique.Really

{-
    The GHC-specific implementation uses  unsafeCoerce
    for reasons of efficiency.

    See  http://apfelmus.nfshost.com/blog/2011/09/04-vault.html
    for the second implementation that doesn't need to
    bypass the type checker.
-}
#if UseGHC
#include "backends/GHC.h"
#else
#include "backends/IORef.hs"
#endif

{-----------------------------------------------------------------------------
    Vault
------------------------------------------------------------------------------}

instance Semigroup (Vault s) where
    (<>) = union

instance Monoid (Vault s) where
    mempty = empty
    mappend = union

-- | The empty vault.
empty :: Vault s
empty = Vault Map.empty

-- | Create a new key for use with a vault.
newKey :: ST s (Key s a)

-- | Lookup the value of a key in the vault.
lookup :: Key s a -> Vault s -> Maybe a

-- | Insert a value for a given key. Overwrites any previous value.
insert :: Key s a -> a -> Vault s -> Vault s

-- | Adjust the value for a given key if it's present in the vault.
adjust :: (a -> a) -> Key s a -> Vault s -> Vault s

-- | Delete a key from the vault.
delete :: Key s a -> Vault s -> Vault s

-- | Merge two vaults (left-biased).
union :: Vault s -> Vault s -> Vault s
union (Vault m) (Vault m') = Vault $ Map.union m m'

{-----------------------------------------------------------------------------
    Locker
------------------------------------------------------------------------------}

-- | Put a single value into a 'Locker'.
lock :: Key s a -> a -> Locker s

-- | Retrieve the value from the 'Locker'.
unlock :: Key s a -> Locker s -> Maybe a
