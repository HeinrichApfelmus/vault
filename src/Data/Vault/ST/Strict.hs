#define LAZINESS Strict
#define IsStrict 1

-- | A persistent store for values of arbitrary types.
-- Variant for the 'ST' monad.
--
-- The 'Vault' type in this module is strict in both keys and values.
#include "ST.h"
