{-# LANGUAGE OverloadedStrings #-}

-- | Per-epoch protocol-parameter snapshot, written by the ledger
-- worker during 'IngestChainHistory' and consumed by
-- 'PreparingForVolatileTail' to backfill @pool_update.deposit@ (first
-- registrations) and @stake_registration.deposit@ (Shelley-Babbage
-- rows whose cert carries no inline deposit).
--
-- The table is LOGGED, not UNLOGGED. A crash must not truncate it
-- because the consumer advances @dbsync_sync_state.last_committed_slot@
-- only after the corresponding epoch's params have been committed
-- here; losing them on crash would leave the relevant deposit columns
-- permanently NULL after resume (the worker re-applies in replay mode
-- which deliberately skips accumulation).
--
-- One row per epoch, keyed on @epoch_no@. INSERT uses
-- @ON CONFLICT DO NOTHING@ so a re-flush after partial crash is a
-- no-op. Truncated at the end of 'PreparingForVolatileTail' once the
-- backfill UPDATEs have run.
module DbSync.Db.Schema.EpochParamPending
  ( -- * Schema type
    EpochParamPending (..)

    -- * Table definition
  , epochParamPendingTableDef
  , epochParamPendingTableName
  ) where

import Cardano.Prelude

import DbSync.Db.Schema.Types
  ( ColumnDef (..)
  , PgType (..)
  , TableDef (..)
  , TableMode (..)
  )
import DbSync.Db.Types (DbLovelace)

-- ---------------------------------------------------------------------------
-- * Schema type
-- ---------------------------------------------------------------------------

-- | One epoch's protocol-param deposit values.
data EpochParamPending = EpochParamPending
  { eppEpochNo         :: !Word64
  , eppStakeKeyDeposit :: !DbLovelace
  , eppPoolDeposit     :: !DbLovelace
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- * Table definition
-- ---------------------------------------------------------------------------

epochParamPendingTableName :: Text
epochParamPendingTableName = "epoch_param_pending"

-- | DDL definition for the @epoch_param_pending@ table.
--
-- LOGGED with a primary key from creation: the consumer's
-- per-epoch flush relies on the PK for @ON CONFLICT (epoch_no) DO
-- NOTHING@ idempotency, and LOGGED is required so the rows survive
-- a crash that happens between flush and sync-state advance.
epochParamPendingTableDef :: TableDef
epochParamPendingTableDef = TableDef
  { tdName    = epochParamPendingTableName
  , tdColumns =
      [ ColumnDef "epoch_no"           PgBigInt  False
      , ColumnDef "stake_key_deposit"  PgNumeric False
      , ColumnDef "pool_deposit"       PgNumeric False
      ]
  , tdMode              = TableLogged
  , tdPrimaryKey        = Just ["epoch_no"]
  , tdChecks            = []
  , tdColumnDefaults    = []
  , tdUniqueConstraints = []
  , tdGeneratedColumns = []
  , tdForeignKeys = []
  }
