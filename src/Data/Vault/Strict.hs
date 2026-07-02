#define LAZINESS Strict
#define MODULE_NAME Data.Vault.Strict
-- BACKEND_GHC is fixed by the import of Data.Vault.ST.*


-- | A persistent store for values of arbitrary types.
--
-- The 'Vault' type in this module is strict in both keys and values.
#include "IO.h"
