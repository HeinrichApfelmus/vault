#define LAZINESS    Lazy
#define MODULE_NAME Data.Vault.ST.Lazy
#if UseGHC
#define BACKEND_GHC 1
#endif

-- | A persistent store for values of arbitrary types.
-- Variant for the 'ST' monad.
--
-- The 'Vault' type in this module is strict in the keys but lazy in the values.
#include "ST.h"
