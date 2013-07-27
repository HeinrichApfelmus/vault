{-----------------------------------------------------------------------------
    Vault
------------------------------------------------------------------------------}

-- | The empty vault.
empty :: Vault s

-- | Create a new key for use with a vault.
newKey :: ST s (Key s a)

-- | Lookup the value of a key in the vault.
lookup :: Key s a -> Vault s -> Maybe a

-- | Insert a value for a given key. Overwrites any previous value.
insert :: Key s a -> a -> Vault s -> Vault s

-- | Adjust the value for a given key if it's present in the vault.
adjust :: (a -> a) -> Key s a -> Vault s -> Vault s

-- | Delete a key from the vault.
delete :: Key s a -> Vault s -> Vault s

-- | Merge two vaults (left-biased).
union :: Vault s -> Vault s -> Vault s

{-----------------------------------------------------------------------------
    Locker
------------------------------------------------------------------------------}

-- | Put a single value into a 'Locker'.
lock :: Key s a -> a -> Locker s

-- | Retrieve the value from the 'Locker'.
unlock :: Key s a -> Locker s -> Maybe a
