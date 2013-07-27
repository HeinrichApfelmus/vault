module Data.Vault.LAZINESS (
    -- * Vault
    Vault, Key,
    empty, newKey, lookup, insert, adjust, delete, union,

    -- * Locker
    Locker,
    lock, unlock,
    ) where

import Prelude hiding (lookup)
import Control.Monad.ST
import qualified Data.Vault.ST.LAZINESS as ST
import Data.Vault.ST.LAZINESS hiding (Vault, Key, Locker, newKey)

-- | A persistent store for values of arbitrary types.
--
-- This variant is the simplest and creates keys in the 'IO' monad.
-- See the module "Data.Vault.ST" if you want to use it with the 'ST' monad instead.
type Vault = ST.Vault RealWorld

-- | Keys for the vault.
type Key = ST.Key RealWorld

-- | Create a new key for use with a vault.
newKey :: IO (Key a)
newKey = stToIO ST.newKey

-- | A persistent store for a single value.
type Locker = ST.Locker RealWorld
