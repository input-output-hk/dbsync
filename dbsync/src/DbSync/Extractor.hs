{-# LANGUAGE RankNTypes #-}

-- | Extractor definition types. An extractor reads 'GenericBlock'
-- values, resolves FK ids via an 'IdResolver', and writes rows via
-- a 'Writer'; the same body runs in both 'IngestChainHistory' and
-- 'FollowingChainTip'.
module DbSync.Extractor
  ( -- * Types
    ExtractorDef (..)
  , ProcessBlockFn
  , cborCaptureEnabled

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
  , blockStakeSlice
  , blockRegisteredPools
  , blockGovExpiresAfter
  , blockCommitteeMembers
  , takeBlockLedgerData

    -- * Accessor classes
  , HasExtractors (..)
  , HasLedgerData (..)
  , HasNetwork (..)

    -- * ExtractState
  , ExtractState (..)
  , freshExtractState
  ) where

import Cardano.Prelude

import Cardano.Ledger.BaseTypes (EpochInterval (..), Network)
import Cardano.Ledger.Coin (Coin)
import qualified Control.Concurrent.Class.MonadSTM.Strict as Strict
import Control.Concurrent.STM.TBQueue (readTBQueue)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as SMaybe

import DbSync.Parser.Types (GenericBlock, GenericTx)
import DbSync.Phase.Ingest.Counter (IdCounters, freshIdCounters)
import DbSync.Db.Schema.Ids (BlockId, PoolHashId, SlotLeaderId, StakeAddressId, TxId, TxOutId)
import DbSync.Db.Schema.Types (TableDef)
import qualified DbSync.Worker.Ledger.StakeDist as Generic
import DbSync.Worker.Ledger.Types
  ( BlockApplyData (..)
  , DepositsMap
  , HasLedgerEnv (..)
  , LedgerEnv (..)
  , ProposedCommitteeMember (..)
  , emptyDepositsMap
  )
import DbSync.Phase.Type (SyncPhase)
import DbSync.Resolver (HasResolver)
import DbSync.Writer (HasWriter)

-- | Access the chain's 'Network' from any environment. Read once
-- at startup from the Shelley genesis and stable for the lifetime
-- of a sync.
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
  { pdName    :: !Text
      -- ^ Unique extractor name (e.g. "core", "utxo", "governance")
  , pdTables  :: ![TableDef]
      -- ^ Table definitions owned by this extractor
  , pdProcess :: ProcessBlockFn
      -- ^ Process a block: extract data, resolve IDs, write rows
  }

-- | Whether the @cbor@ extractor is active. It is the only consumer of a
-- parsed transaction's raw-CBOR field, so the parser uses this to skip
-- building (and retaining) @tx_cbor@ payloads when nothing will read them.
cborCaptureEnabled :: [ExtractorDef] -> Bool
cborCaptureEnabled = any ((== "cbor") . pdName)

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
      -- ^ Pre-assigned id of this block.
  , bcSlotLeaderId :: !SlotLeaderId
      -- ^ Pre-assigned id of this block's slot leader.
  , bcSlotLeaderNew :: !Bool
      -- ^ 'True' when this slot leader was seen for the first time.
  , bcSlotLeaderPoolHashId :: !(Maybe PoolHashId)
      -- ^ Pool-hash FK for Shelley+ blocks; 'Nothing' for Byron and EBBs.
  , bcPrevBlockId  :: !(Maybe BlockId)
      -- ^ Id of the previous block; 'Nothing' for the first block of
      -- the current session.
  , bcGenBlock     :: !GenericBlock
      -- ^ Parsed block payload.
  , bcTxs          :: ![TxContext]
      -- ^ One entry per transaction, in block order.
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
      -- ^ Pre-assigned id of this transaction.
  , tcGenTx  :: !GenericTx
      -- ^ Parsed transaction payload.
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

-- | Per-block ledger output when the ledger feature is on.
data LedgerOutputs = LedgerOutputs
  { loDepositsMap      :: !DepositsMap
      -- ^ Per-tx deposits, keyed by tx-body hash.
  , loStakeKeyDeposit  :: !(Maybe Coin)
      -- ^ Protocol-param stake-key deposit at this block.
  , loPoolDeposit      :: !(Maybe Coin)
      -- ^ Protocol-param pool deposit at this block.
  , loStakeSlice       :: !Generic.StakeSliceRes
      -- ^ Per-block slice of the "mark" stake distribution.
  , loRegisteredPools  :: !(Set.Set ByteString)
      -- ^ Raw pool-hash bytes registered in the ledger before this
      -- block; used to decide the pool_update.active_epoch_no offset.
  , loGovExpiresAfter  :: !(Maybe Word64)
      -- ^ Gov-action lifetime (in epochs) from this block's protocol
      -- params; 'Nothing' outside Conway. Drives
      -- gov_action_proposal.expiration.
  , loCommitteeMembers :: !(Map.Map (ByteString, Word64) [ProposedCommitteeMember])
      -- ^ Full resolved committee per committee-updating proposal in
      -- this block, keyed by @(proposal tx hash, proposal index)@.
  }

-- | Default for the ledger-disabled case.
emptyBlockLedgerData :: BlockLedgerData
emptyBlockLedgerData = LedgerDataOff

-- | All-zero/Nothing 'LedgerOutputs'.
emptyLedgerOutputs :: LedgerOutputs
emptyLedgerOutputs = LedgerOutputs
  { loDepositsMap      = emptyDepositsMap
  , loStakeKeyDeposit  = Nothing
  , loPoolDeposit      = Nothing
  , loStakeSlice       = Generic.NoSlices
  , loRegisteredPools  = Set.empty
  , loGovExpiresAfter  = Nothing
  , loCommitteeMembers = Map.empty
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

-- | Per-block stake slice; 'Generic.NoSlices' when ledger is off
-- and for Byron / pre-Shelley blocks.
blockStakeSlice :: BlockLedgerData -> Generic.StakeSliceRes
blockStakeSlice = \case
  LedgerDataOff   -> Generic.NoSlices
  LedgerDataOn lo -> loStakeSlice lo

-- | Pool hashes registered in the ledger before this block; empty
-- when ledger is off.
blockRegisteredPools :: BlockLedgerData -> Set.Set ByteString
blockRegisteredPools = \case
  LedgerDataOff   -> Set.empty
  LedgerDataOn lo -> loRegisteredPools lo

-- | Gov-action lifetime (epochs) from this block's protocol params;
-- 'Nothing' when ledger is off or outside Conway.
blockGovExpiresAfter :: BlockLedgerData -> Maybe Word64
blockGovExpiresAfter = \case
  LedgerDataOff   -> Nothing
  LedgerDataOn lo -> loGovExpiresAfter lo

-- | Full resolved committee per committee-updating proposal in this
-- block; empty when ledger is off or outside Conway.
blockCommitteeMembers
  :: BlockLedgerData -> Map.Map (ByteString, Word64) [ProposedCommitteeMember]
blockCommitteeMembers = \case
  LedgerDataOff   -> Map.empty
  LedgerDataOn lo -> loCommitteeMembers lo

-- | Drain the worker's next per-block 'BlockApplyData' and project it
-- onto 'BlockLedgerData'. Blocks until one is available; the
-- ledger-OFF arm returns 'emptyBlockLedgerData' without touching any
-- queue.
takeBlockLedgerData :: HasLedgerEnv -> IO BlockLedgerData
takeBlockLedgerData = \case
  LedgerDisabled _   -> pure emptyBlockLedgerData
  LedgerEnabled lenv -> do
    blockData <- Strict.atomically (readTBQueue (leBlockApplyResults lenv))
    pure $ LedgerDataOn LedgerOutputs
      { loDepositsMap      = badDepositsMap blockData
      , loStakeKeyDeposit  = SMaybe.maybe Nothing Just (badStakeKeyDeposit blockData)
      , loPoolDeposit      = SMaybe.maybe Nothing Just (badPoolDeposit blockData)
      , loStakeSlice       = badStakeSlice blockData
      , loRegisteredPools  = badPoolsRegistered blockData
      , loGovExpiresAfter  =
          SMaybe.maybe Nothing (\(EpochInterval n) -> Just (fromIntegral n))
            (badGovExpiresAfter blockData)
      , loCommitteeMembers = badCommitteeMembers blockData
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
-- * ExtractState
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
