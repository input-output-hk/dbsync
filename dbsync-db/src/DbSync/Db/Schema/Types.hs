{-# LANGUAGE OverloadedStrings #-}

-- | Schema definition types.
--
-- Defines 'TableDef' and 'ColumnDef' used to generate CREATE TABLE DDL
-- at runtime. During 'IngestChainHistory', tables are created from these
-- definitions as UNLOGGED with no indexes or constraints.
module DbSync.Db.Schema.Types
  ( -- * Types
    TableDef (..)
  , ColumnDef (..)
  , PgType (..)
  , TableMode (..)
  ) where

import Cardano.Prelude

-- * Types

-- | Whether a table should be created as LOGGED or UNLOGGED.
data TableMode
  = TableLogged    -- ^ Normal table with WAL (for FollowingChainTip)
  | TableUnlogged  -- ^ UNLOGGED table (for IngestChainHistory — no WAL)
  deriving stock (Eq, Show)

-- | PostgreSQL column type.
data PgType
  = PgBigInt        -- ^ BIGINT (int8)
  | PgInteger       -- ^ INTEGER (int4)
  | PgSmallInt      -- ^ SMALLINT (int2)
  | PgText          -- ^ TEXT
  | PgBytea         -- ^ BYTEA
  | PgJsonb         -- ^ JSONB
  | PgBoolean       -- ^ BOOLEAN
  | PgNumeric       -- ^ NUMERIC
  | PgTimestamp      -- ^ TIMESTAMP WITHOUT TIME ZONE
  | PgTimestampTz    -- ^ TIMESTAMP WITH TIME ZONE
  | PgEnum !Text     -- ^ Custom enum type name
  | PgGenerated !Text -- ^ GENERATED ALWAYS AS (expression)
  deriving stock (Eq, Show)

-- | Definition of a single column.
data ColumnDef = ColumnDef
  { cdName     :: !Text      -- ^ Column name
  , cdType     :: !PgType    -- ^ PostgreSQL type
  , cdNullable :: !Bool      -- ^ True if the column allows NULL
  }
  deriving stock (Eq, Show)

-- | Definition of a database table.
-- Used by 'DbSync.Schema.Generate' to produce CREATE TABLE DDL.
data TableDef = TableDef
  { tdName    :: !Text         -- ^ Table name
  , tdColumns :: ![ColumnDef]  -- ^ Column definitions
  , tdMode    :: !TableMode    -- ^ LOGGED vs UNLOGGED
  }
  deriving stock (Eq, Show)
