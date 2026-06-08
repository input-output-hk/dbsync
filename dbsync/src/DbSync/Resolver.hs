{-# LANGUAGE OverloadedStrings #-}

-- | ID resolution interface for the unified extraction pipeline.
--
-- An 'IdResolver' provides the mechanism for obtaining database IDs
-- during block processing. Two implementations exist:
--
-- * 'DbSync.Phase.Ingest.Resolver' — DedupStore\/Counter-based for 'IngestChainHistory'
-- * 'DbSync.Phase.Following.Resolver' — SELECT->INSERT for 'FollowingChainTip'
--
-- Extractors are parameterised by 'IdResolver' so the same extraction
-- logic works in both phases.
module DbSync.Resolver
  ( -- * Types
    IdResolver (..)

    -- * Accessor class
  , HasResolver (..)
  ) where

import Cardano.Prelude

import Data.ByteString.Short (ShortByteString)
import DbSync.Db.Schema.Address (Address)
import DbSync.Db.Schema.Core (SlotLeader)
import DbSync.Db.Schema.EpochBoundary (CostModel)
import DbSync.Db.Schema.Governance
  ( CommitteeHash
  , DrepHash
  , VotingAnchor
  )
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.MultiAsset (MultiAsset)
import DbSync.Db.Schema.Pool (PoolHash)
import DbSync.Db.Schema.ScriptsDatums (Datum, RedeemerData, Script)
import DbSync.Db.Schema.StakeDelegation (StakeAddress)
import DbSync.Db.Types (AnchorType, DbLovelace)
import DbSync.Phase.Ingest.UtxoStore (UtxoTxEntry)
import DbSync.Worker.OffChain.Types (PoolMetadataRef)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | How to obtain database IDs during block processing.
--
-- The @m@ parameter is the effect monad — 'IO' in production,
-- potentially a test monad in tests.
data IdResolver m = IdResolver
  { -- ---------------------------------------------------------------
    -- Core (shared IDs — used by processBlock centrally)
    -- ---------------------------------------------------------------

    -- | Assign the next block ID.
    assignBlockId     :: !(m BlockId)

    -- | Assign the next transaction ID.
  , assignTxId        :: !(m TxId)

    -- | Assign the next transaction output ID.
  , assignTxOutId     :: !(m TxOutId)

    -- | Resolve a slot leader by its hash.
    -- Returns @(SlotLeaderId, isNew)@. When @isNew = True@, the caller
    -- should also write the 'SlotLeader' row via the 'Writer'.
  , resolveSlotLeader :: !(ByteString -> SlotLeader -> m (SlotLeaderId, Bool))

    -- | Look up the previous block's ID by its hash.
  , resolvePrevBlock  :: !(ByteString -> m (Maybe BlockId))

    -- | The most-recently-assigned block ID, or 'Nothing' before the
    -- first block of the current session. Used by the boundary
    -- handler to populate FK columns that reference the boundary
    -- block's @id@.
  , lookupLastBlockId :: !(m (Maybe BlockId))

    -- ---------------------------------------------------------------
    -- UTxO extractor IDs
    -- ---------------------------------------------------------------

    -- | Ingest-only: queue (tx_out_id, raw, derived address) for the
    -- 'AddressResolver' worker, which bulk-fills @tx_out.address_id@
    -- an epoch later. Panics in Follow.
  , recordTxOutAddress           :: !(TxOutId -> ByteString -> Address -> m ())

    -- | As 'recordTxOutAddress' but for @collateral_tx_out@.
  , recordCollateralTxOutAddress :: !(CollateralTxOutId -> ByteString -> Address -> m ())

    -- | Follow-only: resolve raw bytes to an 'AddressId', queuing the
    -- @address@ INSERT on the per-block buffer when the bytes are new.
    -- Callers fill @tx_out.address_id@ at INSERT time rather than
    -- INSERT-then-UPDATE. Panics in Ingest.
  , resolveAddressId :: !(ByteString -> Address -> m AddressId)

  , assignCollateralTxOutId :: !(m CollateralTxOutId)

    -- ---------------------------------------------------------------
    -- MultiAsset extractor IDs
    -- ---------------------------------------------------------------

    -- | Resolve a multi-asset by its (policy ++ name) key.
    -- Key is 'ShortByteString' (unpinned) to avoid pinned ByteString
    -- concatenation in the hot multi-asset lookup path.
    -- Returns @(MultiAssetId, isNew)@.
  , resolveMultiAsset :: !(ShortByteString -> MultiAsset -> m (MultiAssetId, Bool))

    -- ---------------------------------------------------------------
    -- StakeDelegation extractor IDs
    -- ---------------------------------------------------------------

    -- | Resolve a stake address by its credential hash.
    -- Returns @(StakeAddressId, isNew)@.
  , resolveStakeAddress :: !(ByteString -> StakeAddress -> m (StakeAddressId, Bool))

    -- ---------------------------------------------------------------
    -- Pool extractor IDs
    -- ---------------------------------------------------------------

    -- | Resolve a pool hash by its key hash.
    -- Returns @(PoolHashId, isNew)@.
  , resolvePoolHash :: !(ByteString -> PoolHash -> m (PoolHashId, Bool))

    -- | Assign the next pool_update ID.
  , assignPoolUpdateId :: !(m PoolUpdateId)

    -- | Assign the next pool_metadata_ref ID.
  , assignPoolMetadataRefId :: !(m PoolMetadataRefId)

    -- ---------------------------------------------------------------
    -- OffChainPools extractor hook
    -- ---------------------------------------------------------------

    -- | Record that the extractor observed a pool-metadata
    -- registration. The production resolvers leave this as a no-op
    -- — the off-chain pool worker independently polls PG for
    -- @pool_metadata_ref@ rows that lack a result. Test resolvers
    -- capture the call for assertions.
  , enqueuePoolMetaFetch :: !(PoolMetadataRef -> m ())

    -- ---------------------------------------------------------------
    -- EpochSyncStats IDs
    -- ---------------------------------------------------------------

    -- | Assign the next epoch_sync_stats ID.
  , assignEpochSyncStatsId :: !(m EpochSyncStatsId)

    -- ---------------------------------------------------------------
    -- EpochBoundary IDs
    -- ---------------------------------------------------------------

    -- | Resolve a cost_model by its 32-byte canonical hash.
    -- Returns @(CostModelId, isNew)@; the caller writes the
    -- 'CostModel' row when @isNew = True@.
  , resolveCostModel :: !(ByteString -> CostModel -> m (CostModelId, Bool))

    -- ---------------------------------------------------------------
    -- ScriptsDatums extractor IDs
    -- ---------------------------------------------------------------

    -- | Resolve a datum by its 32-byte hash. Returns @(DatumId, isNew)@.
  , resolveDatum :: !(ByteString -> Datum -> m (DatumId, Bool))

    -- | Resolve a script by its hash. Returns @(ScriptId, isNew)@.
  , resolveScript :: !(ByteString -> Script -> m (ScriptId, Bool))

    -- | Resolve a redeemer_data by its 32-byte hash.
    -- Returns @(RedeemerDataId, isNew)@.
  , resolveRedeemerData :: !(ByteString -> RedeemerData -> m (RedeemerDataId, Bool))

    -- | Assign the next redeemer ID.
  , assignRedeemerId :: !(m RedeemerId)

    -- ---------------------------------------------------------------
    -- Governance extractor IDs
    -- ---------------------------------------------------------------

    -- | Resolve a DRep credential. The 'ByteString' is the 28-byte
    -- credential hash for concrete DReps; the abstract
    -- @always_abstain@ / @always_no_confidence@ DReps share a
    -- distinct sentinel (the Bech32 @view@ string encoded as bytes)
    -- so the dedup key is total.
  , resolveDrepHash :: !(ByteString -> DrepHash -> m (DrepHashId, Bool))

    -- | Resolve a committee-hash credential by @(raw, has_script)@.
    -- The pair is encoded as @raw <> [has_script_byte]@ so the
    -- key is a single 'ByteString'.
  , resolveCommitteeHash :: !(ByteString -> CommitteeHash -> m (CommitteeHashId, Bool))

    -- | Resolve a voting anchor by the @(url, data_hash, type)@
    -- natural key. The key is encoded by the caller; the resolver
    -- only sees a single 'ByteString'.
  , resolveVotingAnchor :: !(ByteString -> AnchorType -> VotingAnchor -> m (VotingAnchorId, Bool))

    -- | Assign the next gov_action_proposal ID.
  , assignGovActionProposalId :: !(m GovActionProposalId)

    -- | Assign the next param_proposal ID.
  , assignParamProposalId :: !(m ParamProposalId)

    -- | Assign the next committee ID.
  , assignCommitteeId :: !(m CommitteeId)

    -- | Assign the next constitution ID.
  , assignConstitutionId :: !(m ConstitutionId)

    -- | Assign the next event_info ID.
  , assignEventInfoId :: !(m EventInfoId)

    -- | Look up an existing @gov_action_proposal.id@ by the proposing
    -- tx hash + proposal index. 'Nothing' for cross-block references
    -- that have not been seen yet (or have rolled back). Reads from
    -- the in-process cache in Ingest; SELECTs from PG in Follow.
  , lookupGovActionProposalId
      :: !(ByteString -> Word64 -> m (Maybe GovActionProposalId))

    -- | Stash a freshly written @gov_action_proposal.id@ in the
    -- cache so later blocks' votes can resolve it without a SELECT.
    -- No-op in Follow (cache lives in PG via @SELECT@).
  , recordGovActionProposalId
      :: !(ByteString -> Word64 -> GovActionProposalId -> m ())

    -- | Snapshot the @(committee_id, no_confidence_id, constitution_id)@
    -- triple representing the currently enacted gov state. EpochBoundary
    -- reads this when building the next @epoch_state@ row.
  , readEnactedEpochStateIds
      :: !(m (Maybe Int64, Maybe Int64, Maybe Int64))

    -- | Replace the snapshot read by 'readEnactedEpochStateIds'.
    -- Called by the governance boundary handler after detecting an
    -- enactment.
  , writeEnactedEpochStateIds
      :: !((Maybe Int64, Maybe Int64, Maybe Int64) -> m ())

    -- | Latest @apGovExpiresAfter@ (gov-action lifetime, epochs)
    -- reported by the ledger worker. 'Nothing' pre-Conway. Read by
    -- the per-block proposal pass to compute
    -- @gov_action_proposal.expiration@.
  , readGovExpiresAfter :: !(m (Maybe Word64))

    -- | Stash the latest @apGovExpiresAfter@. Called by the
    -- governance boundary handler.
  , writeGovExpiresAfter :: !(Maybe Word64 -> m ())

    -- ---------------------------------------------------------------
    -- Inline value resolution (Follow path)
    -- ---------------------------------------------------------------

    -- | Look up output values by (producing tx hash, output index).
    -- 'Nothing' for any pair the resolver cannot fulfil. During Ingest,
    -- the value comes from the 'UtxoStore' (hit) or 'Nothing' (
    -- miss, deferred to the post-load resolve).
  , resolveInputValues :: !([(ByteString, Word16)] -> m [Maybe DbLovelace])

    -- | Look up the producing tx's id, the producer-output's tx_out
    -- row id, and the output value in one call. 'Nothing' on miss.
    -- Used by the UTxO extractor to write @tx_in.tx_out_id@ at COPY
    -- time, to enqueue the consumed-by triple keyed by the output's
    -- 'TxOutId', and to accumulate input values for the deposit
    -- calculation. Follow resolves via SQL; Ingest reads from the
    -- in-process 'UtxoStore'.
  , resolveInputUtxo :: !(ByteString -> Word16 -> m (Maybe (TxId, TxOutId, DbLovelace)))

    -- | Record a tx's outputs in the Ingest 'UtxoStore' so later
    -- inputs spending them resolve at COPY time. No-op in Follow.
  , recordTxOutputs :: !(ByteString -> UtxoTxEntry -> m ())

    -- | Buffer a @(producer_tx_out_id, consumer_tx_id)@ pair for the
    -- 'TxOutWorker'. Called by the UTxO extractor on a cache hit; the
    -- worker fans these into a bulk UPDATE against
    -- @tx_out.consumed_by_tx_id@ at the next epoch boundary. No-op
    -- when @utxo.consumed_by_tx_id@ is off and in Follow.
  , recordConsumed :: !(TxOutId -> TxId -> m ())

    -- | Remove a consumed output from the Ingest 'UtxoStore' so the
    -- table tracks the live UTxO set rather than chain history.
    -- Called for regular inputs and for phase-2 failed collateral.
    -- No-op in Follow.
  , deleteCachedUtxo :: !(ByteString -> Word16 -> m ())
  }

-- ---------------------------------------------------------------------------
-- * Accessor class
-- ---------------------------------------------------------------------------

-- | Access the (IO-effecting) ID resolver from any environment.
--
-- The resolver is fixed to 'IO' here because the production resolvers
-- ('mkIngestResolver', and the future 'FollowingChainTip' SELECT/INSERT
-- resolver) both run in 'IO'. Test environments can store an 'IO'-backed
-- mock; nothing in the codebase needs an arbitrary @m@ at the env layer.
class HasResolver env where
  getResolver :: env -> IdResolver IO
