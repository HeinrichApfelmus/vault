-- | A specialized strict 'Map' type suitable for 'Unique' keys.
--
-- This type is useful in the rare case that 'Data.HashMap' is not available,
-- e.g. because you are not compiling with GHC.
module Data.Unique.Really.Map (
    UniqueMap,
    elems, empty, insert, delete
    ) where

import Data.Unique.Really

#if UseGHC

import qualified Data.HashMap.Strict as Map

-- | A map from 'Unique' to values of type @a@.
newtype UniqueMap a = UniqueMap (Map.HashMap Unique a)

empty                    = UniqueMap Map.empty
elems      (UniqueMap m) = Map.elems m
insert k x (UniqueMap m) = UniqueMap (Map.insert k x m)
delete k   (UniqueMap m) = UniqueMap (Map.delete k m)

#else

import qualified Data.IntMap.Strict as Map

-- | A map from 'Unique' to values of type @a@.
newtype UniqueMap a = UniqueMap (Map.IntMap a)

empty                    = UniqueMap Map.empty
elems      (UniqueMap m) = Map.elems m
insert k x (UniqueMap m) = UniqueMap (Map.insert (hashUnique k) x m)
delete k   (UniqueMap m) = UniqueMap (Map.delete (hashUnique k) m)

#endif

-- | The empty map.
empty :: UniqueMap a

-- | Return all elements of the map.
elems :: UniqueMap a -> [a]

-- | Insert a new key/value pair in the map.
--  If the key is already present in the map, the value is replaced with the new one.
insert :: Unique -> a -> UniqueMap a -> UniqueMap a

-- | Delete a key and its value from the map. 
delete :: Unique -> UniqueMap a -> UniqueMap a
