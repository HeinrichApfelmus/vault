{-----------------------------------------------------------------------------
    vault
------------------------------------------------------------------------------}
module Data.Vault.ST.Pure_Lazy where

import Prelude hiding (lookup)
import Data.Functor
import Data.IORef
import Control.Applicative
import Control.Monad.ST

import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.ST.Unsafe

import Data.Unique

import qualified Data.Map.Lazy as Map
type Map = Map.Map

{-----------------------------------------------------------------------------
    Locker
------------------------------------------------------------------------------}
data Key s a  = Key    !Unique (IORef (Maybe a))
data Locker s = Locker !Unique (IO ())

lock :: Key s a -> a -> Locker s
lock (Key u ref) x = Locker u $ writeIORef ref $ Just x

unlock :: Key s a -> Locker s -> Maybe a
unlock (Key _ ref) (Locker _ m) = unsafePerformIO $ do
    m
    mx <- readIORef ref     -- FIXME: race condition!
    writeIORef ref Nothing
    return mx

{-----------------------------------------------------------------------------
    Vault
------------------------------------------------------------------------------}
-- implemented as a collection of lockers
newtype Vault s = Vault (Map Unique (Locker s))

empty  = Vault Map.empty

newKey :: ST s (Key s a)
newKey = Control.Monad.ST.Unsafe.unsafeIOToST $
    Key <$> newUnique <*> newIORef Nothing

lookup :: Key s a -> Vault s -> Maybe a
lookup key@(Key k _)   (Vault m) = unlock key =<< Map.lookup k m

insert key@(Key k _) x (Vault m) = Vault $ Map.insert k (lock key x) m

adjust :: (a -> a) -> Key s a -> Vault s -> Vault s
adjust f key@(Key k _) (Vault m) = Vault $ Map.update f' k m
    where f' = fmap (lock key . f) . unlock key

delete (Key k _) (Vault m)  = Vault $ Map.delete k m
union  (Vault m) (Vault m') = Vault $ Map.union m m'
