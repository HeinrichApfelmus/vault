#define LAZINESS Lazy

-- | A persistent store for values of arbitrary types.
--
-- The 'Vault' type in this module is strict in the keys but lazy in the values.
#include "IO.h"
