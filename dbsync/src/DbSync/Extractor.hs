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

    -- * Per-block ledger output
  , BlockLedgerData (..)
  , emptyBlockLedgerData

    -- * Accessor classes
  , HasExtractors (..)
  , HasLedgerData (..)

    -- * Re-exports (for ExtractState used by IngestResolver)
  , ExtractState (..)
  , freshExtractState
  ) where

import Cardano.Prelude

import Cardano.Ledger.BaseTypes (Network)
import Cardano.Ledger.Coin (Coin)

import DbSync.Block.Types (GenericBlock, GenericTx)
import DbSync.Id.Counter (IdCounters, freshIdCounters)
import DbSync.Db.Schema.Ids (BlockId, PoolHashId, SlotLeaderId, StakeAddressId, TxId, TxOutId)
import DbSync.Db.Schema.Types (TableDef)
import DbSync.Ledger.Types (DepositsMap, emptyDepositsMap)
import DbSync.Db.Phase (SyncPhase)
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
  , bcSlotLeaderPoolHashId :: !(Maybe PoolHashId)
      -- ^ Pool-hash FK for Shelley+ blocks; 'Nothing' for Byron and EBBs.
  , bcPrevBlockId  :: !(Maybe BlockId)
  , bcGenBlock     :: !GenericBlock
  , bcTxs          :: ![TxContext]
  , bcNetwork      :: !Network
      -- ^ Chain network ID; drives the HRP on Bech32 stake / reward
      -- encodings produced by extractors.
  , bcLedgerData   :: !BlockLedgerData
      -- ^ Worker output for this block. Empty when ledger is OFF.
  , bcSyncPhase    :: !SyncPhase
      -- ^ Drives Ingest vs Follow tx-row construction inside the core extractor.
  }

-- | A transaction with pre-assigned shared IDs.
data TxContext = TxContext
  { tcTxId   :: !TxId
  , tcGenTx  :: !GenericTx
  , tcOutIds :: ![TxOutId]
      -- ^ One TxOutId per output, same length and order as @txOutputs gtx@.
  , tcOutStakeIds :: ![Maybe StakeAddressId]
      -- ^ One stake-address FK per output, in the same order. 'Nothing'
      -- when the address carries no inline stake credential (Byron,
      -- enterprise, pointer, reward).
  }

-- ---------------------------------------------------------------------------
-- * Per-block ledger output
-- ---------------------------------------------------------------------------

-- | One block's worth of ledger-worker output, consumed by extractors.
data BlockLedgerData = BlockLedgerData
  { bldLedgerEnabled   :: !Bool
      -- ^ False disables the other fields; extractors fall through.
  , bldDepositsMap     :: !DepositsMap
      -- ^ Per-tx deposits, keyed by tx-body hash. Plain txs aren't here.
  , bldStakeKeyDeposit :: !(Maybe Coin)
      -- ^ Protocol-param stake-key deposit at this block.
  , bldPoolDeposit     :: !(Maybe Coin)
      -- ^ Protocol-param pool deposit at this block.
  }

-- | Default for the ledger-disabled case.
emptyBlockLedgerData :: BlockLedgerData
emptyBlockLedgerData = BlockLedgerData
  { bldLedgerEnabled   = False
  , bldDepositsMap     = emptyDepositsMap
  , bldStakeKeyDeposit = Nothing
  , bldPoolDeposit     = Nothing
  }

-- ---------------------------------------------------------------------------
-- * Accessor classes
-- ---------------------------------------------------------------------------

-- | Read the active extractor list from the env.
class HasExtractors env where
  getExtractors :: env -> [ExtractorDef]

-- | Fetch per-block ledger data. Ingest+ON blocks until the worker
-- has applied the block; ledger-OFF returns 'emptyBlockLedgerData'.
class HasLedgerData env where
  getLedgerData :: env -> GenericBlock -> IO BlockLedgerData

-- ---------------------------------------------------------------------------
-- * ExtractState (used by IngestResolver)
-- ---------------------------------------------------------------------------

-- | Mutable state threaded during 'IngestChainHistory'.
--
-- Contains the monotonic ID counters and tracking state that ensure
-- stable, deterministic ID assignment.
--
-- Deduplication maps ('DedupMaps') live separately as mutable hash
-- tables — they are passed directly to the resolver, not through
-- this 'IORef'-wrapped record. This avoids CAS-loop overhead for
-- dedup operations and eliminates path-copying GC pressure.
--
-- NOT used during 'FollowingChainTip' — the 'IdResolver' handles
-- ID assignment via PostgreSQL directly.
data ExtractState = ExtractState
  { esIdCounters  :: !IdCounters
      -- ^ Per-table monotonic ID counters
  , esLastBlockId :: !(Maybe Int64)
      -- ^ ID of the most recently processed block (for previous_id).
      --   'Nothing' before any block has been processed.
  }
  deriving stock (Eq, Show)

-- | Initial state for a brand-new sync — every counter at 1, no
-- previously-seen block.
freshExtractState :: ExtractState
freshExtractState = ExtractState
  { esIdCounters  = freshIdCounters
  , esLastBlockId = Nothing
  }
