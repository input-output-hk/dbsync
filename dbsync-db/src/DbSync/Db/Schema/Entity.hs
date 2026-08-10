{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The 'Key' type family, which maps a table type to its ID newtype
-- (e.g. @Key Block = BlockId@), plus the 'Entity' pair that carries a key
-- beside its row. Each schema module declares its own 'Key' instance.
module DbSync.Db.Schema.Entity
  ( -- * Types
    Entity (..)
  , Key
  ) where

import Cardano.Prelude

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | 'IngestChainHistory' assigns the key in-process. 'FollowingChainTip'
-- takes the key from PostgreSQL @RETURNING id@.
data Entity record = Entity
  { entityKey :: !(Key record)
      -- ^ Unused. No module imports 'Entity'; they import 'Key' alone.
  , entityVal :: !record
      -- ^ Unused. No module imports 'Entity'; they import 'Key' alone.
  }

-- | Injective type family mapping each table type to its ID newtype.
--
-- The injectivity annotation @k -> a@ lets GHC infer the record type from
-- the key type.
type family Key a = k | k -> a

deriving stock instance (Eq (Key record), Eq record) => Eq (Entity record)
deriving stock instance (Show (Key record), Show record) => Show (Entity record)
