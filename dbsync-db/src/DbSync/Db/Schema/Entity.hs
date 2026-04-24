{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Entity wrapper pairing a primary key with its row data.
--
-- The 'Entity' type and 'Key' type family form the backbone of the
-- schema type system: every table row is represented as @Entity T@ where
-- @Key T@ is the corresponding newtype ID (e.g. @Key Block = BlockId@).
--
-- Type family instances linking each table to its key are declared in
-- the module that defines the table (e.g. 'DbSync.Db.Schema.Core').
module DbSync.Db.Schema.Entity
  ( -- * Types
    Entity (..)
  , Key
  ) where

import Cardano.Prelude

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | A database entity: primary key plus row data.
--
-- @
-- Entity BlockId block   -- a block row with its database ID
-- Entity TxId   tx       -- a tx row with its database ID
-- @
--
-- During 'IngestChainHistory', IDs are pre-assigned in-process.
-- During 'FollowingChainTip', IDs come from PostgreSQL @RETURNING id@.
data Entity record = Entity
  { entityKey :: !(Key record)  -- ^ The primary key
  , entityVal :: !record        -- ^ The row data
  }

-- | Injective type family mapping each table type to its ID newtype.
--
-- The injectivity annotation @k -> a@ means GHC can infer the record
-- type from the key type, enabling functions like:
--
-- @
-- insertBlock :: Entity Block -> IO ()
-- -- GHC knows Key Block = BlockId, so entityKey returns a BlockId
-- @
--
-- Instances are declared alongside each table type in the schema modules.
type family Key a = k | k -> a

-- Derive instances that work for any Entity whose Key and record have the right instances.
deriving stock instance (Eq (Key record), Eq record) => Eq (Entity record)
deriving stock instance (Show (Key record), Show record) => Show (Entity record)
