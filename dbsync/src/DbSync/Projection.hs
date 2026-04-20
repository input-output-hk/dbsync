{- |
Module      : DbSync.Projection
Description : Projection definition types for modular data extraction.

A projection is a self-contained unit of extraction logic that reads
'GenericBlock' values and produces COPY rows grouped by table. Each
projection declares its schema ('TableDef'), dependencies on other
projections, and a pure extraction function.
-}
module DbSync.Projection
  ( -- * Types
    ProjectionDef (..)
  , ExtractFn
  , ExtractState (..)
  , RowBatches (..)
  ) where

import Cardano.Prelude

import Data.Map.Strict (Map)

import DbSync.Block.Types (GenericBlock)
import DbSync.Id.Counter (IdCounters)
import DbSync.Id.DedupMap (DedupMaps)
import DbSync.Db.Schema.Types (TableDef)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Definition of a single projection.
--
-- Projections are the unit of modular extraction — each one owns a set
-- of tables and a pure extraction function that turns a block into
-- COPY-ready rows.
data ProjectionDef = ProjectionDef
  { pdName         :: !Text
      -- ^ Unique projection name (e.g. "core", "utxo", "governance")
  , pdVersion      :: !Int
      -- ^ Schema version; bump when the projection's tables change
  , pdDependencies :: ![(Text, Int)]
      -- ^ @(projectionName, minimumVersion)@ pairs this projection depends on
  , pdTables       :: ![TableDef]
      -- ^ Table definitions owned by this projection
  , pdExtract      :: !ExtractFn
      -- ^ Pure extraction function: block + state -> rows + updated state
  }

-- | The extraction function signature shared by all projections.
--
-- Given a 'GenericBlock' and the current 'ExtractState', produce
-- a batch of COPY rows ('RowBatches') and the updated state.
-- Must be pure — all side effects happen in the caller.
type ExtractFn = GenericBlock -> ExtractState -> (RowBatches, ExtractState)

-- | Mutable state threaded through extraction across blocks.
--
-- Contains the monotonic ID counters and deduplication maps that
-- ensure stable, deterministic ID assignment during 'IngestChainHistory'.
data ExtractState = ExtractState
  { esIdCounters :: !IdCounters
      -- ^ Per-table monotonic ID counters
  , esDedupMaps  :: !DedupMaps
      -- ^ Dedup maps for lookup/reference tables
  }
  deriving stock (Show)

-- | COPY rows grouped by table name.
--
-- Each entry maps a table name to a list of pre-encoded COPY rows
-- (one 'ByteString' per row, tab-separated and newline-terminated).
newtype RowBatches = RowBatches { unRowBatches :: Map Text [ByteString] }
  deriving newtype (Semigroup, Monoid)
  deriving stock (Show)
