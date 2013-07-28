module Data.Vault.ST.LAZINESS (
    -- * Vault
    Vault, Key,
    empty, newKey, lookup, insert, adjust, delete, union,

    -- * Locker
    Locker,
    lock, unlock,
    ) where

import Data.Monoid (Monoid(..))
import Prelude hiding (lookup)
import Control.Applicative hiding (empty)
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
#include "GHC.hs"
#else
#include "IORef.hs"
#endif

#include "API.hs"

empty = Vault Map.empty

union (Vault m) (Vault m') = Vault $ Map.union m m'

instance Monoid (Vault s) where
    mempty = empty
    mappend = union
