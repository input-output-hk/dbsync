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
  , ForeignKey (..)
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
--
-- Generated columns ('GENERATED ALWAYS AS (...) STORED') are not
-- represented here — their underlying SQL type lives in 'cdType'
-- (e.g. 'PgBigInt' for an @earned_epoch@ column) and the generation
-- expression in 'tdGeneratedColumns'. Keeping the two pieces in
-- separate per-table fields avoids the duplicate path that an
-- in-band 'PgGenerated' constructor would create.
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
  deriving stock (Eq, Show)

-- | Definition of a single column.
data ColumnDef = ColumnDef
  { cdName     :: !Text      -- ^ Column name
  , cdType     :: !PgType    -- ^ PostgreSQL type
  , cdNullable :: !Bool      -- ^ True if the column allows NULL
  }
  deriving stock (Eq, Show)

-- | An outgoing foreign-key reference from one table to another.
--
-- Declared per 'TableDef' so the rollback cascade can derive its
-- per-FK-family delete lists from the schema rather than maintaining
-- parallel hand-coded tables. No PG-level @REFERENCES@ constraint is
-- emitted from this — it's purely metadata for code that walks the
-- schema. Column names stay strings; that mirrors 'ColumnDef' and
-- avoids dragging in a typed-column abstraction we don't have
-- anywhere else.
data ForeignKey = ForeignKey
  { fkColumn       :: !Text  -- ^ This table's FK column.
  , fkParentTable  :: !Text  -- ^ Parent table's name.
  , fkParentColumn :: !Text  -- ^ Parent table's column (usually @"id"@).
  }
  deriving stock (Eq, Show)

-- | Definition of a database table.
-- Used by 'DbSync.Schema.Generate' to produce CREATE TABLE DDL.
--
-- The optional-shaped fields — 'tdPrimaryKey', 'tdChecks',
-- 'tdColumnDefaults', 'tdUniqueConstraints', 'tdGeneratedColumns' —
-- are empty for the extractor data tables (which are UNLOGGED,
-- constraint-free, and get indexes only in 'PreparingForVolatileTail').
-- They exist for the small number of tables that need
-- LOGGED-from-day-one semantics with constraints — currently
-- @dbsync_sync_state@ — and to carry per-table metadata that is
-- consumed later (unique constraints in 'PreparingForVolatileTail',
-- generated-column expressions in DDL emission).
data TableDef = TableDef
  { tdName              :: !Text
      -- ^ Table name
  , tdColumns           :: ![ColumnDef]
      -- ^ Column definitions
  , tdMode              :: !TableMode
      -- ^ LOGGED vs UNLOGGED
  , tdPrimaryKey        :: !(Maybe [Text])
      -- ^ Optional primary key. 'Just cols' emits @PRIMARY KEY (col1, …)@
      -- as a table-level constraint. 'Nothing' for extractor tables
      -- (PK added later in 'PreparingForVolatileTail').
  , tdChecks            :: ![Text]
      -- ^ Zero or more table-level @CHECK@ constraint expressions,
      -- each emitted verbatim as @CHECK (expr)@.
  , tdColumnDefaults    :: ![(Text, Text)]
      -- ^ Per-column @DEFAULT@ expressions, keyed by column name.
      -- Columns not listed get no default clause. Values are emitted
      -- verbatim after the type, so e.g. @("updated_at", "now()")@
      -- yields @"updated_at" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()@.
  , tdUniqueConstraints :: ![NonEmpty Text]
      -- ^ Table-level @UNIQUE (col1, …)@ constraints, each a
      -- non-empty list of column names. Not emitted at
      -- @CREATE TABLE@ time during 'IngestChainHistory'; consumed by
      -- 'PreparingForVolatileTail' indexing.
  , tdGeneratedColumns  :: ![(Text, Text)]
      -- ^ Per-column @GENERATED ALWAYS AS (expr) STORED@ definitions,
      -- keyed by column name. Listed columns are excluded from the
      -- COPY column list in 'DbSync.Db.Loader.Connection.beginStream' so
      -- PostgreSQL computes them on insert.
  , tdForeignKeys       :: ![ForeignKey]
      -- ^ Outgoing FK references. Consumed by the
      -- 'FollowingChainTip' rollback cascade to compute per-FK-family
      -- delete lists. Empty for tables with no incoming references to
      -- the rollback's parent tables (block, tx, tx_out, pool_update).
  }
  deriving stock (Eq, Show)
