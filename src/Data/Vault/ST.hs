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
    -- * Boxes
    Box,
    toBox, fromBox,
    ) where

import Prelude hiding (lookup)
import Data.Monoid hiding (Any)
import Data.Functor
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IORef
import Control.Monad.ST

import GHC.Exts (Any)   -- ghc specific tricks
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

toAny :: a -> Any
toAny = unsafeCoerce

fromAny :: Any -> a
fromAny = unsafeCoerce

-- | A typed, persistent store for values of arbitrary types.
-- 
-- This variant has more complex types so that you can create keys in the 'ST' monad.
-- See the module "Data.Vault" if you'd like to use a simpler version with the 'IO' monad.
-- You can also use both variants simultaneously; they share a single representation.
newtype Vault s = Vault (IntMap Any)
-- | Keys for the vault.
newtype Key s a = Key Int

instance Monoid (Vault s) where
    mempty = empty
    mappend = union

-- | The empty vault.
empty :: Vault s
empty = Vault IntMap.empty

{-# NOINLINE nextKey #-}
nextKey :: IORef (Key s a)
nextKey = unsafePerformIO $ newIORef (Key 0)

-- | Create a new key for use with a vault.
newKey :: ST s (Key s a)
newKey = unsafeIOToST . atomicModifyIORef nextKey $ \k@(Key i) ->
    let k' = Key (i+1)
    in k' `seq` (k', k)

-- | Lookup the value of a key in the vault.
lookup :: Key s a -> Vault s -> Maybe a
lookup (Key k) (Vault m) = fromAny <$> IntMap.lookup k m

-- | Insert a value for a given key. Overwrites any previous value.
insert :: Key s a -> a -> Vault s -> Vault s
insert (Key k) x (Vault m) = Vault $ IntMap.insert k (toAny x) m

-- | Adjust the value for a given key if it's present in the vault.
adjust :: (a -> a) -> Key s a -> Vault s -> Vault s
adjust f (Key k) (Vault m) = Vault $ IntMap.adjust f' k m
    where f' = toAny . f . fromAny

-- | Delete a key from the vault.
delete :: Key s a -> Vault s -> Vault s
delete (Key k) (Vault m) = Vault $ IntMap.delete k m

-- | Merge two vaults (left-biased).
union :: Vault s -> Vault s -> Vault s
union (Vault m) (Vault m') = Vault $ IntMap.union m m'

-- | An efficient implementation of a single-element @Vault s@.
data Box s = Box !Int Any

-- | @toBox k a@ is analogous to @insert k a empty@.
toBox :: Key s a -> a -> Box s
toBox (Key k) = Box k . toAny

-- | @fromBox k a@ is analogous to @lookup k a@.
fromBox :: Key s a -> Box s -> Maybe a
fromBox (Key k) (Box k' a)
  | k == k' = Just $ fromAny a
  | otherwise = Nothing
