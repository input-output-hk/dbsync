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
  , ParentRef (..)
  , PgType (..)
  , TableMode (..)
  , TableColumn (..)

    -- * Ownership-graph queries
  , childrenOf
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
  | PgTimestamp     -- ^ TIMESTAMP WITHOUT TIME ZONE
  | PgTimestampTz   -- ^ TIMESTAMP WITH TIME ZONE
  | PgTextArray     -- ^ TEXT[]
  deriving stock (Eq, Show)

-- | Definition of a single column.
data ColumnDef = ColumnDef
  { cdName     :: !Text      -- ^ Column name
  , cdType     :: !PgType    -- ^ PostgreSQL type
  , cdNullable :: !Bool      -- ^ True if the column allows NULL
  }
  deriving stock (Eq, Show)

-- | An ownership edge: rows of this table belong to a row of
-- 'prParentTable' and must die with it. Drives the resume trim, the
-- rollback cascade, and the @FOREIGN KEY@ constraints created in
-- 'PreparingForVolatileTail'.
--
-- Only ownership is declared. A reference to a deduplicated row —
-- @tx_out.inline_datum_id@, @tx_in.redeemer_id@, @ma_tx_out.ident@ — is
-- shared between transactions rather than owned by one, so it is
-- deliberately neither cascaded nor constrained.
--
-- Column names stay strings; that mirrors 'ColumnDef' and avoids
-- dragging in a typed-column abstraction we don't have anywhere else.
data ParentRef = ParentRef
  { prColumn       :: !Text  -- ^ This table's referencing column.
  , prParentTable  :: !Text
  , prParentColumn :: !Text  -- ^ Parent table's column (usually @"id"@).
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
      -- ^ Optional primary-key column list. 'Just cols' emits
      -- @PRIMARY KEY (col1, …)@ in the @CREATE TABLE@ DDL (LOGGED
      -- tables) and matches the index built in
      -- 'PreparingForVolatileTail'. 'Nothing' is the conventional
      -- default for extractor data tables: no PK clause is emitted
      -- at @CREATE TABLE@ time (UNLOGGED COPY pays no index cost),
      -- and the post-load index builder treats it as @["id"]@.
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
  , tdIdentityColumns   :: ![Text]
      -- ^ Columns emitted as @BIGINT GENERATED BY DEFAULT AS IDENTITY@,
      -- letting PostgreSQL allocate values from a backing sequence.
      -- Used for leaf-table @id@ columns that nothing else FKs into,
      -- so the row layer doesn't have to thread an 'IdCounter' through
      -- the resolver just to satisfy NOT NULL. Listed columns are
      -- excluded from the COPY column list the same way generated
      -- columns are.
  , tdParentRefs        :: ![ParentRef]
      -- ^ Rows this table's rows belong to. Drives both the resume
      -- trim and the rollback cascade; empty only when no column
      -- points at @block@, @tx@, @tx_out@ or @pool_update@.
  }
  deriving stock (Eq, Show)

-- | A column reference that carries its owning 'TableDef'. Built
-- from the per-table column records (e.g. @poolHashCols.phcHashRaw@)
-- and consumed by the SQL-builder helpers in 'DbSync.Db.Sql.Refs'.
-- Pairing the table with the name makes mismatched references a
-- type error rather than a runtime panic.
data TableColumn = TableColumn
  { tcTable :: !TableDef
  , tcName  :: !Text
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Ownership-graph queries
-- ---------------------------------------------------------------------------

-- | Tables declaring an ownership edge to @parentTable@, each paired
-- with its referencing column.
childrenOf :: [TableDef] -> Text -> [(Text, Text)]
childrenOf tables parentTable =
  [ (tdName td, prColumn pr)
  | td <- tables
  , pr <- tdParentRefs td
  , prParentTable pr == parentTable
  ]
