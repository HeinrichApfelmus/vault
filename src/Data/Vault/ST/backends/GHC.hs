-- This implementation is specific to GHC
-- und uses  unsafeCoerce  for reasons of efficiency.
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.HashMap.LAZINESS as Map
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

#if __GLASGOW_HASKELL__ >= 708
type role Vault nominal
type role Key nominal nominal
#endif

newKey = STUnsafe.unsafeIOToST $ Key <$> newUnique

lookup (Key k) (Vault m) = fromAny <$> Map.lookup k m

insert (Key k) x (Vault m) = Vault $ Map.insert k (toAny x) m

adjust f (Key k) (Vault m) = Vault $ Map.adjust f' k m
    where f' = toAny . f . fromAny

delete (Key k) (Vault m) = Vault $ Map.delete k m

{-----------------------------------------------------------------------------
    Locker
------------------------------------------------------------------------------}
data Locker s = Locker !Unique !Any

lock (Key k) = Locker k . toAny

unlock (Key k) (Locker k' a)
  | k == k' = Just $ fromAny a
  | otherwise = Nothing
