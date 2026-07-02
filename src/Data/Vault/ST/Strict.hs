#define LAZINESS    Strict
#define MODULE_NAME Data.Vault.ST.Strict
#define IsStrict 1
#if UseGHC
#define BACKEND_GHC 1
#endif

-- | A persistent store for values of arbitrary types.
-- Variant for the 'ST' monad.
--
-- The 'Vault' type in this module is strict in both keys and values.
#include "ST.h"
