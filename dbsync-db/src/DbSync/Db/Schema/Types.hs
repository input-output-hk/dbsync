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
--
-- The three optional-shaped fields — 'tdPrimaryKey', 'tdChecks',
-- 'tdColumnDefaults' — are empty for the extractor data tables
-- (which are UNLOGGED, constraint-free, and get indexes only in
-- 'PreparingForChainTip'). They exist for the small number of tables
-- that need LOGGED-from-day-one semantics with constraints — currently
-- @dbsync_sync_state@.
data TableDef = TableDef
  { tdName           :: !Text
      -- ^ Table name
  , tdColumns        :: ![ColumnDef]
      -- ^ Column definitions
  , tdMode           :: !TableMode
      -- ^ LOGGED vs UNLOGGED
  , tdPrimaryKey     :: !(Maybe [Text])
      -- ^ Optional primary key. 'Just cols' emits @PRIMARY KEY (col1, …)@
      -- as a table-level constraint. 'Nothing' for extractor tables
      -- (PK added later in 'PreparingForChainTip').
  , tdChecks         :: ![Text]
      -- ^ Zero or more table-level @CHECK@ constraint expressions,
      -- each emitted verbatim as @CHECK (expr)@.
  , tdColumnDefaults :: ![(Text, Text)]
      -- ^ Per-column @DEFAULT@ expressions, keyed by column name.
      -- Columns not listed get no default clause. Values are emitted
      -- verbatim after the type, so e.g. @("updated_at", "now()")@
      -- yields @"updated_at" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()@.
  }
  deriving stock (Eq, Show)
