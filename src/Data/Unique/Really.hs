-- | An abstract interface to a unique symbol generator.
module Data.Unique.Really (
    Unique, newUnique, hashUnique,
    ) where

#if UseGHC
import Data.Unique.Really.GHC
#else
import Data.Unique.Really.Any
#endif
