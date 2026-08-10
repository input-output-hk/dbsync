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
  , scriptsDatumsEnabled
  , utxoEnabled

    -- * Block context (pre-assigned shared IDs)
  , BlockContext (..)
  , TxContext (..)
  , redeemerIdAt

    -- * Per-block ledger output
  , BlockLedgerData (..)
  , LedgerOutputs (..)
  , emptyBlockLedgerData
  , emptyLedgerOutputs
  , blockDepositsMap
  , blockStakeKeyDeposit
  , blockPoolDeposit
  , blockPrices
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

import Cardano.Ledger.Alonzo.Scripts (Prices)
import Cardano.Ledger.BaseTypes (EpochInterval (..), Network)
import Cardano.Ledger.Coin (Coin)
import qualified Control.Concurrent.Class.MonadSTM.Strict as Strict
import Control.Concurrent.STM.TBQueue (readTBQueue)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as SMaybe

import DbSync.Parser.Types (GenericBlock, GenericTx)
import DbSync.Phase.Ingest.Counter (IdCounters, freshIdCounters)
import DbSync.Db.Schema.Ids (BlockId, PoolHashId, RedeemerId, SlotLeaderId, StakeAddressId, TxId, TxOutId)
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

-- | The sync reads the 'Network' once at startup from the Shelley
-- genesis; it is stable for the lifetime of the process.
class HasNetwork env where
  getNetwork :: env -> Network

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | One extractor: the tables it owns, plus the function that fills them.
data ExtractorDef = ExtractorDef
  { pdName    :: !Text
      -- ^ Unique name, e.g. "core", "utxo", "governance".
  , pdTables  :: ![TableDef]
      -- ^ Tables this extractor owns.
  , pdProcess :: ProcessBlockFn
      -- ^ Reads the block, resolves FK ids, writes rows.
  }

-- | The @cbor@ extractor is the only consumer of a parsed transaction's
-- raw-CBOR field. The parser reads this to skip building (and
-- retaining) @tx_cbor@ payloads that nothing will read.
cborCaptureEnabled :: [ExtractorDef] -> Bool
cborCaptureEnabled = any ((== "cbor") . pdName)

-- | Gates redeemer id assignment. When the extractor is off, nothing
-- writes @redeemer@ rows and the table's sequence may not exist, so
-- the pipeline must draw no ids from it.
scriptsDatumsEnabled :: [ExtractorDef] -> Bool
scriptsDatumsEnabled = any ((== "scripts_datums") . pdName)

-- | Also gates the @tx_in@ table, which carries the redeemer
-- back-references.
utxoEnabled :: [ExtractorDef] -> Bool
utxoEnabled = any ((== "utxo") . pdName)

-- | Polymorphic over any env with 'HasResolver', 'HasWriter' and
-- 'HasNetwork', so one body works in 'IngestM' (COPY-backed) and
-- 'FollowM' (INSERT-backed).
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

-- | A block with its shared ids already assigned.
--
-- The pipeline assigns 'BlockId', 'SlotLeaderId', per-tx 'TxId' and
-- per-output 'TxOutId' centrally, so extractors do not depend on each
-- other's execution order.
data BlockContext = BlockContext
  { bcBlockId      :: !BlockId
  , bcSlotLeaderId :: !SlotLeaderId
  , bcSlotLeaderNew :: !Bool
      -- ^ 'True' when this slot leader was seen for the first time.
  , bcSlotLeaderPoolHashId :: !(Maybe PoolHashId)
      -- ^ Pool-hash FK for Shelley+ blocks; 'Nothing' for Byron and EBBs.
  , bcPrevBlockId  :: !(Maybe BlockId)
      -- ^ 'Nothing' for the first block of the current session.
  , bcGenBlock     :: !GenericBlock
  , bcTxs          :: ![TxContext]
      -- ^ One entry per transaction, in block order.
  , bcNetwork      :: !Network
      -- ^ Drives the HRP on the Bech32 stake and reward encodings.
  , bcLedgerData   :: !BlockLedgerData
      -- ^ Ledger-worker output for this block.
  , bcSyncPhase    :: !SyncPhase
      -- ^ Selects Ingest or Follow tx-row construction in the core
      -- extractor.
  }

-- | A transaction with its shared ids already assigned.
data TxContext = TxContext
  { tcTxId   :: !TxId
  , tcGenTx  :: !GenericTx
  , tcOutIds :: ![TxOutId]
      -- ^ One TxOutId per output, same length and order as @txOutputs gtx@.
  , tcOutStakeIds :: ![Maybe StakeAddressId]
      -- ^ One stake-address FK per output, in the same order. 'Nothing'
      -- when the address carries no inline stake credential (Byron,
      -- enterprise, pointer, reward).
  , tcRedeemerIds :: ![RedeemerId]
      -- ^ One id per entry of @txRedeemers gtx@, in the same order.
      -- Empty when the @scripts_datums@ extractor is off or the tx
      -- failed phase-2 validation — no redeemer rows exist to point at.
  }

-- | The 'Word64' is a parser annotation: a position into
-- @txRedeemers@. An annotation with no matching id (@scripts_datums@
-- off) gives 'Nothing', so the FK cell stays NULL.
redeemerIdAt :: TxContext -> Maybe Word64 -> Maybe RedeemerId
redeemerIdAt tc mIx = do
  ix <- mIx
  listToMaybe (drop (fromIntegral ix) (tcRedeemerIds tc))

-- ---------------------------------------------------------------------------
-- * Per-block ledger output
-- ---------------------------------------------------------------------------

-- | One block's worth of ledger-worker output.
--
-- 'LedgerDataOff' is the only shape available when the ledger feature
-- is off. The per-block fields are then unconstructible, so the type
-- rules out the "off with populated fields" state.
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
  , loPrices           :: !(Maybe Prices)
      -- ^ Plutus execution prices at this block; 'Nothing' pre-Alonzo.
      -- Drives @redeemer.fee@.
  , loStakeSlice       :: !Generic.StakeSliceRes
      -- ^ Per-block slice of the "mark" stake distribution.
  , loRegisteredPools  :: !(Set.Set ByteString)
      -- ^ Raw pool-hash bytes the ledger registered before this block.
      -- Decides the @pool_update.active_epoch_no@ offset.
  , loGovExpiresAfter  :: !(Maybe Word64)
      -- ^ Gov-action lifetime (in epochs) from this block's protocol
      -- params; 'Nothing' outside Conway. Drives
      -- @gov_action_proposal.expiration@.
  , loCommitteeMembers :: !(Map.Map (ByteString, Word64) [ProposedCommitteeMember])
      -- ^ Full resolved committee per committee-updating proposal in
      -- this block, keyed by @(proposal tx hash, proposal index)@.
  }

emptyBlockLedgerData :: BlockLedgerData
emptyBlockLedgerData = LedgerDataOff

emptyLedgerOutputs :: LedgerOutputs
emptyLedgerOutputs = LedgerOutputs
  { loDepositsMap      = emptyDepositsMap
  , loStakeKeyDeposit  = Nothing
  , loPoolDeposit      = Nothing
  , loPrices           = Nothing
  , loStakeSlice       = Generic.NoSlices
  , loRegisteredPools  = Set.empty
  , loGovExpiresAfter  = Nothing
  , loCommitteeMembers = Map.empty
  }

blockDepositsMap :: BlockLedgerData -> DepositsMap
blockDepositsMap = \case
  LedgerDataOff   -> emptyDepositsMap
  LedgerDataOn lo -> loDepositsMap lo

blockStakeKeyDeposit :: BlockLedgerData -> Maybe Coin
blockStakeKeyDeposit = \case
  LedgerDataOff   -> Nothing
  LedgerDataOn lo -> loStakeKeyDeposit lo

blockPoolDeposit :: BlockLedgerData -> Maybe Coin
blockPoolDeposit = \case
  LedgerDataOff   -> Nothing
  LedgerDataOn lo -> loPoolDeposit lo

blockPrices :: BlockLedgerData -> Maybe Prices
blockPrices = \case
  LedgerDataOff   -> Nothing
  LedgerDataOn lo -> loPrices lo

blockStakeSlice :: BlockLedgerData -> Generic.StakeSliceRes
blockStakeSlice = \case
  LedgerDataOff   -> Generic.NoSlices
  LedgerDataOn lo -> loStakeSlice lo

blockRegisteredPools :: BlockLedgerData -> Set.Set ByteString
blockRegisteredPools = \case
  LedgerDataOff   -> Set.empty
  LedgerDataOn lo -> loRegisteredPools lo

blockGovExpiresAfter :: BlockLedgerData -> Maybe Word64
blockGovExpiresAfter = \case
  LedgerDataOff   -> Nothing
  LedgerDataOn lo -> loGovExpiresAfter lo

blockCommitteeMembers
  :: BlockLedgerData -> Map.Map (ByteString, Word64) [ProposedCommitteeMember]
blockCommitteeMembers = \case
  LedgerDataOff   -> Map.empty
  LedgerDataOn lo -> loCommitteeMembers lo

-- | Take the worker's next per-block 'BlockApplyData'. Blocks until
-- one arrives; the ledger-off arm returns 'emptyBlockLedgerData'
-- without touching a queue.
takeBlockLedgerData :: HasLedgerEnv -> IO BlockLedgerData
takeBlockLedgerData = \case
  LedgerDisabled _   -> pure emptyBlockLedgerData
  LedgerEnabled lenv -> do
    blockData <- Strict.atomically (readTBQueue (leBlockApplyResults lenv))
    pure $ LedgerDataOn LedgerOutputs
      { loDepositsMap      = badDepositsMap blockData
      , loStakeKeyDeposit  = SMaybe.maybe Nothing Just (badStakeKeyDeposit blockData)
      , loPoolDeposit      = SMaybe.maybe Nothing Just (badPoolDeposit blockData)
      , loPrices           = SMaybe.maybe Nothing Just (badPrices blockData)
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

class HasExtractors env where
  getExtractors :: env -> [ExtractorDef]

-- | With the ledger on, this blocks until the worker applies the
-- block; with it off, it returns 'emptyBlockLedgerData'.
class HasLedgerData env where
  getLedgerData :: env -> GenericBlock -> IO BlockLedgerData

-- ---------------------------------------------------------------------------
-- * ExtractState
-- ---------------------------------------------------------------------------

-- | Mutable state for 'IngestChainHistory', which assigns ids from
-- monotonic counters. 'FollowingChainTip' does not use it; there the
-- 'IdResolver' assigns ids through PostgreSQL.
--
-- Dedup state ('DedupStores') sits on the shared 'LsmSession' and goes
-- straight to the resolver, not through this record.
data ExtractState = ExtractState
  { esIdCounters     :: !IdCounters
  , esLastBlockId    :: !(Maybe Int64)
      -- ^ Id of the last processed block, for @previous_id@. 'Nothing'
      --   before the first block.
  , esCostModelCache :: !(Map ByteString Int64)
      -- ^ Hash to @cost_model.id@. The boot path fills it from the
      --   @cost_model@ table when a sync resumes; it starts empty on a
      --   fresh sync.
  , esGovActionProposalCache :: !(Map (ByteString, Word64) Int64)
      -- ^ @(proposing tx hash, proposal index) -> gov_action_proposal.id@,
      --   so votes in later blocks resolve their target proposal
      --   without a SELECT. The boot path rebuilds it from @tx.hash@
      --   and @gov_action_proposal.index@.
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
      --   from the ledger worker. The proposal pass reads it to
      --   compute @gov_action_proposal.expiration@.
  }
  deriving stock (Eq, Show)

-- | Every counter starts at 1, with no previous block.
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
