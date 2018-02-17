-- | An abstract interface to a unique symbol generator.
module Data.Unique.Really (
    Unique, newUnique, hashUnique,
    ) where

import Control.Applicative ((<$>))
import Data.Hashable

#if UseGHC

import Control.Exception (evaluate)
import qualified Data.Unique
import System.Mem.StableName

-- | An abstract unique value.
-- Values of type 'Unique' may be compared for equality
-- and hashed into Int.
--
-- Note: Unlike the symbols from "Data.Unique", the symbols from this
-- module do not become equal after reloads in the GHC interpreter!
newtype Unique = Unique (StableName Data.Unique.Unique) deriving (Eq)

newUnique = do
    x <- Data.Unique.newUnique
    _ <- evaluate x
    Unique <$> makeStableName x

hashUnique (Unique s) = hashStableName s

#else

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE refNumber #-}
refNumber :: IORef Integer
refNumber = unsafePerformIO $ newIORef 0

newNumber = atomicModifyIORef' refNumber $ \x -> let x' = x+1 in (x', x')

-- | An abstract unique value.
-- Values of type 'Unique' may be compared for equality
-- and hashed into Int.
--
-- NOTE: You haven't compiled this module with GHC.
-- The functionality will be identitcal to "Data.Unique".
newtype Unique = Unique Integer deriving (Eq,Ord)

newUnique = Unique <$> newNumber
hashUnique (Unique s) = fromIntegral s


#endif

-- | Creates a new object of type 'Unique'.
-- The value returned will not compare equal to any other
-- value of type 'Unique' returned by previous calls to 'newUnique'.
-- There is no limit on the number of times you may call this function.
newUnique  :: IO Unique

-- | Hashes a 'Unique' into an 'Int'.
-- Two Uniques may hash to the same value, although in practice this is unlikely.
-- The 'Int' returned makes a good hash key.
hashUnique :: Unique -> Int

instance Hashable Unique where hashWithSalt s = hashWithSalt s . hashUnique
