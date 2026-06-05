{-# LANGUAGE RankNTypes #-}

{- |
Module      : DbSync.Extractor
Description : Extractor definition types for modular data extraction.

An extractor is a self-contained unit of extraction logic that reads
'GenericBlock' values, resolves foreign key IDs via an 'IdResolver',
and writes rows via a 'Writer'. The same extraction code works in
both 'IngestChainHistory' (COPY + DedupStores) and 'FollowingChainTip'
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
  , LedgerOutputs (..)
  , emptyBlockLedgerData
  , emptyLedgerOutputs
  , blockDepositsMap
  , blockStakeKeyDeposit
  , blockPoolDeposit

    -- * Accessor classes
  , HasExtractors (..)
  , HasLedgerData (..)
  , HasNetwork (..)

    -- * Re-exports (for ExtractState used by IngestResolver)
  , ExtractState (..)
  , freshExtractState
  ) where

import Cardano.Prelude

import Cardano.Ledger.BaseTypes (Network)
import Cardano.Ledger.Coin (Coin)

import DbSync.Parser.Types (GenericBlock, GenericTx)
import DbSync.Phase.Ingest.Counter (IdCounters, freshIdCounters)
import DbSync.Db.Schema.Ids (BlockId, PoolHashId, SlotLeaderId, StakeAddressId, TxId, TxOutId)
import DbSync.Db.Schema.Types (TableDef)
import DbSync.Worker.Ledger.Types (DepositsMap, emptyDepositsMap)
import DbSync.Phase.Type (SyncPhase)
import DbSync.Resolver (HasResolver)
import DbSync.Writer (HasWriter)

-- ---------------------------------------------------------------------------
-- * HasNetwork
-- ---------------------------------------------------------------------------

-- | Access the chain's 'Network' (mainnet vs testnet) from any
-- environment. Read once at startup from the Shelley genesis and
-- never changes for the lifetime of a sync.
--
-- Lives here (rather than in "DbSync.App.Env") because 'ProcessBlockFn'
-- needs the constraint and the env definitions in "DbSync.App.Env"
-- already depend on this module via 'HasExtractors'\/'HasLedgerData'.
class HasNetwork env where
  getNetwork :: env -> Network

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
-- Polymorphic over any env that satisfies 'HasResolver',
-- 'HasWriter' and 'HasNetwork', so the same body works in 'IngestM'
-- (COPY-backed) and 'FollowM' (INSERT-backed). The pre-assigned
-- shared IDs and the per-block worker output ride on the
-- 'BlockContext'.
type ProcessBlockFn =
  forall env m.
  ( HasResolver env
  , HasWriter env
  , HasNetwork env
  , MonadReader env m
  , MonadIO m
  )
  => BlockContext -> m ()

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
--
-- 'LedgerDataOff' is the only valid shape when the ledger feature is
-- disabled; the per-block 'Maybe' fields are unconstructible in that
-- case so the impossible "off-with-populated-fields" state is ruled
-- out at the type level.
data BlockLedgerData
  = LedgerDataOff
  | LedgerDataOn !LedgerOutputs
  deriving stock (Eq, Show)

-- | Per-block ledger output when the ledger feature is on.
data LedgerOutputs = LedgerOutputs
  { loDepositsMap     :: !DepositsMap
      -- ^ Per-tx deposits, keyed by tx-body hash. Plain txs aren't here.
  , loStakeKeyDeposit :: !(Maybe Coin)
      -- ^ Protocol-param stake-key deposit at this block.
  , loPoolDeposit     :: !(Maybe Coin)
      -- ^ Protocol-param pool deposit at this block.
  }
  deriving stock (Eq, Show)

-- | Default for the ledger-disabled case.
emptyBlockLedgerData :: BlockLedgerData
emptyBlockLedgerData = LedgerDataOff

-- | All-zero/Nothing 'LedgerOutputs'. Convenient base for tests and
-- for the worker before any deposit observation has landed.
emptyLedgerOutputs :: LedgerOutputs
emptyLedgerOutputs = LedgerOutputs
  { loDepositsMap     = emptyDepositsMap
  , loStakeKeyDeposit = Nothing
  , loPoolDeposit     = Nothing
  }

-- | Per-tx deposits map; 'emptyDepositsMap' when ledger is off.
blockDepositsMap :: BlockLedgerData -> DepositsMap
blockDepositsMap = \case
  LedgerDataOff   -> emptyDepositsMap
  LedgerDataOn lo -> loDepositsMap lo

-- | Protocol-param stake-key deposit; 'Nothing' when ledger is off.
blockStakeKeyDeposit :: BlockLedgerData -> Maybe Coin
blockStakeKeyDeposit = \case
  LedgerDataOff   -> Nothing
  LedgerDataOn lo -> loStakeKeyDeposit lo

-- | Protocol-param pool deposit; 'Nothing' when ledger is off.
blockPoolDeposit :: BlockLedgerData -> Maybe Coin
blockPoolDeposit = \case
  LedgerDataOff   -> Nothing
  LedgerDataOn lo -> loPoolDeposit lo

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
-- Deduplication state ('DedupStores') lives separately on top of
-- the shared 'LsmSession' and is passed directly to the resolver,
-- not through this 'IORef'-wrapped record.
--
-- NOT used during 'FollowingChainTip' — the 'IdResolver' handles
-- ID assignment via PostgreSQL directly.
data ExtractState = ExtractState
  { esIdCounters     :: !IdCounters
      -- ^ Per-table monotonic ID counters
  , esLastBlockId    :: !(Maybe Int64)
      -- ^ ID of the most recently processed block (for previous_id).
      --   'Nothing' before any block has been processed.
  , esCostModelCache :: !(Map ByteString Int64)
      -- ^ Hash → cost_model.id dedup cache. Populated at boot from
      --   the @cost_model@ table when resuming an existing sync;
      --   empty on a fresh start.
  , esGovActionProposalCache :: !(Map (ByteString, Word64) Int64)
      -- ^ @(proposing tx hash, proposal index) -> gov_action_proposal.id@
      --   so vote rows in later blocks can resolve their target
      --   proposal without a SELECT. Populated as proposals are
      --   written; rebuilt at boot from @tx.hash@ +
      --   @gov_action_proposal.index@.
  , esCurrentCommitteeId    :: !(Maybe Int64)
      -- ^ @committee.id@ representing the currently enacted committee,
      --   stamped onto @epoch_state.committee_id@ at the next boundary.
      --   Updated by the governance boundary handler when an
      --   @UpdateCommittee@ proposal enacts.
  , esCurrentNoConfidenceId :: !(Maybe Int64)
      -- ^ @gov_action_proposal.id@ of the latest enacted
      --   no-confidence action; stamped onto
      --   @epoch_state.no_confidence_id@ at the next boundary.
  , esCurrentConstitutionId :: !(Maybe Int64)
      -- ^ @constitution.id@ representing the currently enacted
      --   constitution; stamped onto @epoch_state.constitution_id@
      --   at the next boundary.
  , esGovExpiresAfter       :: !(Maybe Word64)
      -- ^ Latest @apGovExpiresAfter@ (gov-action lifetime in epochs)
      --   reported by the ledger worker. Used by the proposal pass
      --   to compute @gov_action_proposal.expiration@.
  }
  deriving stock (Eq, Show)

-- | Initial state for a brand-new sync — every counter at 1, no
-- previously-seen block.
freshExtractState :: ExtractState
freshExtractState = ExtractState
  { esIdCounters             = freshIdCounters
  , esLastBlockId            = Nothing
  , esCostModelCache         = mempty
  , esGovActionProposalCache = mempty
  , esCurrentCommitteeId     = Nothing
  , esCurrentNoConfidenceId  = Nothing
  , esCurrentConstitutionId  = Nothing
  , esGovExpiresAfter        = Nothing
  }
