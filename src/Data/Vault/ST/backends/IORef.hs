import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map.LAZINESS as Map
type Map = Map.Map

{-----------------------------------------------------------------------------
    Locker
------------------------------------------------------------------------------}
data Key s a  = Key    !Unique (IORef (Maybe a))
data Locker s = Locker !Unique (IO ())

#if IsStrict
lock (Key u ref) x = x `seq` (Locker u $ writeIORef ref $ Just x)
#else
lock (Key u ref) x = Locker u $ writeIORef ref $ Just x
#endif

unlock (Key k ref) (Locker k' m)
    | k == k' = unsafePerformIO $ do
        m
        readIORef ref     -- FIXME: race condition!
    | otherwise = Nothing

{-----------------------------------------------------------------------------
    Vault
------------------------------------------------------------------------------}
-- implemented as a collection of lockers
newtype Vault s = Vault (Map Unique (Locker s))

newKey = STUnsafe.unsafeIOToST $ Key <$> newUnique <*> newIORef Nothing

lookup key@(Key k _)   (Vault m) = unlock key =<< Map.lookup k m

insert key@(Key k _) x (Vault m) = Vault $ Map.insert k (lock key x) m

adjust f key@(Key k _) (Vault m) = Vault $ Map.update f' k m
    where f' = fmap (lock key . f) . unlock key

delete (Key k _) (Vault m)  = Vault $ Map.delete k m
