{-----------------------------------------------------------------------------
    vault
------------------------------------------------------------------------------}
module Data.Vault.ST.GHC_Lazy where

import Prelude hiding (lookup)
import Data.Functor
import Data.IORef
import Control.Monad.ST
#if MIN_VERSION_base(4,4,0)
import Control.Monad.ST.Unsafe as STUnsafe
#else
import Control.Monad.ST as STUnsafe
#endif

import Data.Unique.Really

-- This implementation is specific to GHC
-- und uses  unsafeCoerce  for reasons of efficiency.
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.HashMap.Lazy as Map
type Map = Map.HashMap

toAny :: a -> Any
toAny = unsafeCoerce

fromAny :: Any -> a
fromAny = unsafeCoerce

{-----------------------------------------------------------------------------
    Vault
------------------------------------------------------------------------------}
newtype Vault s = Vault (Map Unique Any)
newtype Key s a = Key Unique

empty :: Vault s
empty = Vault Map.empty

newKey :: ST s (Key s a)
newKey = STUnsafe.unsafeIOToST $ Key <$> newUnique

lookup :: Key s a -> Vault s -> Maybe a
lookup (Key k) (Vault m) = fromAny <$> Map.lookup k m

insert :: Key s a -> a -> Vault s -> Vault s
insert (Key k) x (Vault m) = Vault $ Map.insert k (toAny x) m

adjust :: (a -> a) -> Key s a -> Vault s -> Vault s
adjust f (Key k) (Vault m) = Vault $ Map.adjust f' k m
    where f' = toAny . f . fromAny

delete (Key k) (Vault m) = Vault $ Map.delete k m

union (Vault m) (Vault m') = Vault $ Map.union m m'

{-----------------------------------------------------------------------------
    Locker
------------------------------------------------------------------------------}
data Locker s = Locker !Unique !Any

lock :: Key s a -> a -> Locker s
lock (Key k) = Locker k . toAny

unlock :: Key s a -> Locker s -> Maybe a
unlock (Key k) (Locker k' a)
  | k == k' = Just $ fromAny a
  | otherwise = Nothing
