{- |
Module      : DbSync.Extractor
Description : Extractor definition types for modular data extraction.

An extractor is a self-contained unit of extraction logic that reads
'GenericBlock' values, resolves foreign key IDs via an 'IdResolver',
and writes rows via a 'Writer'. The same extraction code works in
both 'IngestChainHistory' (COPY + DedupMaps) and 'FollowingChainTip'
(INSERT + DB queries).
-}
module DbSync.Extractor
  ( -- * Types
    ExtractorDef (..)
  , ProcessBlockFn

    -- * Block context (pre-assigned shared IDs)
  , BlockContext (..)
  , TxContext (..)

    -- * Re-exports (for ExtractState used by IngestResolver)
  , ExtractState (..)
  ) where

import Cardano.Prelude

import DbSync.Block.Types (GenericBlock, GenericTx)
import DbSync.Id.Counter (IdCounters)
import DbSync.Id.DedupMap (DedupMaps)
import DbSync.Db.Schema.Ids (BlockId, SlotLeaderId, TxId, TxOutId)
import DbSync.Db.Schema.Types (TableDef)
import DbSync.Resolver (IdResolver)
import DbSync.Writer (Writer)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Definition of a single extractor.
--
-- Extractors are the unit of modular extraction — each one owns a set
-- of tables and a processing function that extracts data from a block,
-- resolves foreign key IDs, and writes rows.
data ExtractorDef = ExtractorDef
  { pdName         :: !Text
      -- ^ Unique extractor name (e.g. "core", "utxo", "governance")
  , pdVersion      :: !Int
      -- ^ Schema version; bump when the extractor's tables change
  , pdDependencies :: ![(Text, Int)]
      -- ^ @(extractorName, minimumVersion)@ pairs this extractor depends on
  , pdTables       :: ![TableDef]
      -- ^ Table definitions owned by this extractor
  , pdProcess      :: ProcessBlockFn
      -- ^ Process a block: extract data, resolve IDs, write rows
  }

-- | Process a single block through this extractor.
--
-- Parameterised by 'IdResolver' (for per-extractor ID assignment)
-- and 'Writer' (where rows go). Receives a 'BlockContext' with
-- pre-assigned shared IDs (BlockId, TxId, TxOutId).
type ProcessBlockFn = IdResolver IO -> Writer IO -> BlockContext -> IO ()

-- ---------------------------------------------------------------------------
-- * Block context (pre-assigned shared IDs)
-- ---------------------------------------------------------------------------

-- | A block with pre-assigned shared IDs.
--
-- The pipeline assigns 'BlockId', 'SlotLeaderId', per-tx 'TxId',
-- and per-output 'TxOutId' centrally. Extractors consume these
-- without needing to know about each other's execution order.
data BlockContext = BlockContext
  { bcBlockId      :: !BlockId
  , bcSlotLeaderId :: !SlotLeaderId
  , bcSlotLeaderNew :: !Bool
      -- ^ 'True' when this slot leader was seen for the first time
  , bcPrevBlockId  :: !(Maybe BlockId)
  , bcGenBlock     :: !GenericBlock
  , bcTxs          :: ![TxContext]
  }

-- | A transaction with pre-assigned shared IDs.
data TxContext = TxContext
  { tcTxId   :: !TxId
  , tcGenTx  :: !GenericTx
  , tcOutIds :: ![TxOutId]
      -- ^ Pre-assigned TxOutId for each output, in order.
      -- @length tcOutIds == length (txOutputs tcGenTx)@
  }

-- ---------------------------------------------------------------------------
-- * ExtractState (used by IngestResolver)
-- ---------------------------------------------------------------------------

-- | Mutable state threaded during 'IngestChainHistory'.
--
-- Contains the monotonic ID counters, deduplication maps, and
-- tracking state that ensure stable, deterministic ID assignment.
-- NOT used during 'FollowingChainTip' — the 'IdResolver' handles
-- ID assignment via PostgreSQL directly.
data ExtractState = ExtractState
  { esIdCounters  :: !IdCounters
      -- ^ Per-table monotonic ID counters
  , esDedupMaps   :: !DedupMaps
      -- ^ Dedup maps for lookup/reference tables
  , esLastBlockId :: !(Maybe Int64)
      -- ^ ID of the most recently processed block (for previous_id).
      --   'Nothing' before any block has been processed.
  }
  deriving stock (Eq, Show)
