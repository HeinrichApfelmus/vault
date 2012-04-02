{-----------------------------------------------------------------------------
    vault
------------------------------------------------------------------------------}
module Data.Vault.ST_GHC where

import Prelude hiding (lookup)
import Data.Functor
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IORef
import Control.Monad.ST

import System.IO.Unsafe (unsafePerformIO)

-- This implementation is specific to GHC
-- und uses  unsafeCoerce  for reasons of efficiency.
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

toAny :: a -> Any
toAny = unsafeCoerce

fromAny :: Any -> a
fromAny = unsafeCoerce

{-----------------------------------------------------------------------------
    Vault
------------------------------------------------------------------------------}
newtype Vault s = Vault (IntMap Any)
newtype Key s a = Key Int

empty :: Vault s
empty = Vault IntMap.empty

{-# NOINLINE nextKey #-}
nextKey :: IORef (Key s a)
nextKey = unsafePerformIO $ newIORef (Key 0)

newKey :: ST s (Key s a)
newKey = unsafeIOToST . atomicModifyIORef nextKey $ \k@(Key i) ->
    let k' = Key (i+1)
    in k' `seq` (k', k)

lookup :: Key s a -> Vault s -> Maybe a
lookup (Key k) (Vault m) = fromAny <$> IntMap.lookup k m

insert :: Key s a -> a -> Vault s -> Vault s
insert (Key k) x (Vault m) = Vault $ IntMap.insert k (toAny x) m

adjust :: (a -> a) -> Key s a -> Vault s -> Vault s
adjust f (Key k) (Vault m) = Vault $ IntMap.adjust f' k m
    where f' = toAny . f . fromAny

delete (Key k) (Vault m) = Vault $ IntMap.delete k m

union (Vault m) (Vault m') = Vault $ IntMap.union m m'

{-----------------------------------------------------------------------------
    Locker
------------------------------------------------------------------------------}
data Locker s = Locker !Int Any

lock :: Key s a -> a -> Locker s
lock (Key k) = Locker k . toAny

unlock :: Key s a -> Locker s -> Maybe a
unlock (Key k) (Locker k' a)
  | k == k' = Just $ fromAny a
  | otherwise = Nothing
