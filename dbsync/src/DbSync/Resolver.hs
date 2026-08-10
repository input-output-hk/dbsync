{-# LANGUAGE OverloadedStrings #-}

-- | How extractors obtain database ids during block processing.
--
-- 'DbSync.Phase.Ingest.Resolver' backs the record with dedup stores
-- and counters; 'DbSync.Phase.Following.Resolver' backs it with
-- SELECT->INSERT. Extractors take an 'IdResolver', so one extractor
-- body runs in both phases.
module DbSync.Resolver
  ( -- * Types
    IdResolver (..)

    -- * Accessor class
  , HasResolver (..)
  ) where

import Cardano.Prelude

import Data.ByteString.Short (ShortByteString)
import DbSync.Db.Schema.Address (Address)
import DbSync.Db.Schema.Core (PoolHash, SlotLeader, StakeAddress)
import DbSync.Db.Schema.EpochBoundary (CostModel)
import DbSync.Db.Schema.Governance
  ( CommitteeHash
  , DrepHash
  , VotingAnchor
  )
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.MultiAsset (MultiAsset)
import DbSync.Db.Schema.ScriptsDatums (Datum, RedeemerData, Script)
import DbSync.Db.Types (AnchorType, DbLovelace)
import DbSync.Phase.Ingest.UtxoStore (UtxoTxEntry)
import DbSync.Worker.OffChain.Types (PoolMetadataRef, VotingAnchorRef)

-- ---------------------------------------------------------------------------
-- * Types
-- ---------------------------------------------------------------------------

-- | Every @resolve*@ field returns @(id, isNew)@; the caller writes the
-- row itself when @isNew@ is 'True'. Every @assign*@ field allocates a
-- fresh id and writes nothing.
data IdResolver m = IdResolver
  { -- Core ids, assigned centrally by processBlock
    assignBlockId     :: !(m BlockId)
  , assignTxId        :: !(m TxId)
  , assignTxOutId     :: !(m TxOutId)
  , resolveSlotLeader :: !(ByteString -> SlotLeader -> m (SlotLeaderId, Bool))
  , resolvePrevBlock  :: !(ByteString -> m (Maybe BlockId))

    -- | The last id 'assignBlockId' gave out. 'Nothing' before the
    -- first block of the session. The boundary handler stamps it onto
    -- FK columns that reference the boundary block.
  , lookupLastBlockId :: !(m (Maybe BlockId))

    -- UTxO ids

    -- | Ingest-only: queue @(tx_out_id, raw, resolved stake id)@ for
    -- the 'AddressResolver' worker, which bulk-fills
    -- @tx_out.address_id@ an epoch later. Panics in Follow.
  , recordTxOutAddress           :: !(TxOutId -> ByteString -> Maybe StakeAddressId -> m ())

    -- | As 'recordTxOutAddress' but for @collateral_tx_out@.
  , recordCollateralTxOutAddress :: !(CollateralTxOutId -> ByteString -> Maybe StakeAddressId -> m ())

    -- | Follow-only: the caller fills @tx_out.address_id@ at INSERT
    -- time instead of INSERT-then-UPDATE. Panics in Ingest.
  , resolveAddressId :: !(ByteString -> Address -> m AddressId)

  , assignCollateralTxOutId :: !(m CollateralTxOutId)

    -- MultiAsset ids

    -- | The @policy ++ name@ key is 'ShortByteString' (unpinned) to
    -- keep pinned 'ByteString' concatenation out of this hot path.
  , resolveMultiAsset :: !(ShortByteString -> MultiAsset -> m (MultiAssetId, Bool))

    -- StakeDelegation ids

  , resolveStakeAddress :: !(ByteString -> StakeAddress -> m (StakeAddressId, Bool))

    -- Pool ids

  , resolvePoolHash :: !(ByteString -> PoolHash -> m (PoolHashId, Bool))

    -- | Look up a pool hash without inserting. 'Nothing' means the key
    -- was never registered as a pool, so a slot leader that bears it
    -- is a genesis-key delegate.
  , resolvePoolHashQuery :: !(ByteString -> m (Maybe PoolHashId))

  , assignPoolUpdateId :: !(m PoolUpdateId)
  , assignPoolMetadataRefId :: !(m PoolMetadataRefId)

    -- OffChain hooks

    -- | No-op in both production resolvers: the off-chain pool worker
    -- polls PG for @pool_metadata_ref@ rows that lack a result. Test
    -- resolvers capture the call for assertions.
  , enqueuePoolMetaFetch :: !(PoolMetadataRef -> m ())

    -- | No-op in both production resolvers: the off-chain vote worker
    -- polls PG for @voting_anchor@ rows that lack a result. Test
    -- resolvers capture the call for assertions.
  , enqueueVoteMetaFetch :: !(VotingAnchorRef -> m ())

    -- EpochSyncStats ids

  , assignEpochSyncStatsId :: !(m EpochSyncStatsId)

    -- EpochBoundary ids

    -- | The key is the 32-byte canonical hash of the cost model.
  , resolveCostModel :: !(ByteString -> CostModel -> m (CostModelId, Bool))

    -- ScriptsDatums ids

  , resolveDatum :: !(ByteString -> Datum -> m (DatumId, Bool))
  , resolveScript :: !(ByteString -> Script -> m (ScriptId, Bool))
  , resolveRedeemerData :: !(ByteString -> RedeemerData -> m (RedeemerDataId, Bool))
  , assignRedeemerId :: !(m RedeemerId)

    -- | Fill @redeemer.script_hash@ for the spend redeemers among the
    -- given ids, reading the payment credential off the spent output's
    -- address. The pipeline calls this once per block after every
    -- extractor runs, so the @tx_in@ and @redeemer@ rows it joins are
    -- already queued. No-op in Ingest: @tx_in.tx_out_id@ is unresolved
    -- there, and 'DbSync.Phase.Preparing.Backfill' covers the whole
    -- range in one pass instead.
  , fillSpendScriptHashes :: !([RedeemerId] -> m ())

    -- Governance ids

    -- | The 'ByteString' is the 28-byte credential hash for concrete
    -- DReps. The abstract @always_abstain@ and @always_no_confidence@
    -- DReps each use a distinct sentinel (the Bech32 @view@ string as
    -- bytes) so the dedup key is total.
  , resolveDrepHash :: !(ByteString -> DrepHash -> m (DrepHashId, Bool))

    -- | The caller encodes the @(raw, has_script)@ key as
    -- @raw <> [has_script_byte]@ to keep it a single 'ByteString'.
  , resolveCommitteeHash :: !(ByteString -> CommitteeHash -> m (CommitteeHashId, Bool))

    -- | The caller encodes the @(url, data_hash, type)@ natural key;
    -- the resolver sees one 'ByteString'.
  , resolveVotingAnchor :: !(ByteString -> AnchorType -> VotingAnchor -> m (VotingAnchorId, Bool))

  , assignGovActionProposalId :: !(m GovActionProposalId)
  , assignParamProposalId :: !(m ParamProposalId)
  , assignCommitteeId :: !(m CommitteeId)
  , assignConstitutionId :: !(m ConstitutionId)
  , assignEventInfoId :: !(m EventInfoId)

    -- | Look up @gov_action_proposal.id@ by proposing tx hash and
    -- proposal index. 'Nothing' for a cross-block reference the sync
    -- has not seen, or that rolled back. Ingest reads the in-process
    -- cache; Follow SELECTs from PG.
  , lookupGovActionProposalId
      :: !(ByteString -> Word64 -> m (Maybe GovActionProposalId))

    -- | Stash a freshly written @gov_action_proposal.id@ so votes in
    -- later blocks resolve it without a SELECT. No-op in Follow.
  , recordGovActionProposalId
      :: !(ByteString -> Word64 -> GovActionProposalId -> m ())

    -- | The @(committee_id, no_confidence_id, constitution_id)@ triple
    -- of the currently enacted gov state. EpochBoundary reads it when
    -- it builds the next @epoch_state@ row.
  , readEnactedEpochStateIds
      :: !(m (Maybe Int64, Maybe Int64, Maybe Int64))

    -- | Replace the snapshot 'readEnactedEpochStateIds' returns. The
    -- governance boundary handler calls this after an enactment.
  , writeEnactedEpochStateIds
      :: !((Maybe Int64, Maybe Int64, Maybe Int64) -> m ())

    -- | Latest @apGovExpiresAfter@ (gov-action lifetime, in epochs)
    -- from the ledger worker. 'Nothing' pre-Conway. The per-block
    -- proposal pass reads it to compute
    -- @gov_action_proposal.expiration@.
  , readGovExpiresAfter :: !(m (Maybe Word64))

  , writeGovExpiresAfter :: !(Maybe Word64 -> m ())

    -- Input resolution

    -- | Look up output values by @(producing tx hash, output index)@.
    -- 'Nothing' for any pair the resolver cannot fulfil; in Ingest a
    -- 'UtxoStore' miss defers the value to the post-load resolve.
  , resolveInputValues :: !([(ByteString, Word16)] -> m [Maybe DbLovelace])

    -- | Look up the producing tx id, the producer output's @tx_out@
    -- row id, and the output value in one call. The UTxO extractor
    -- uses it to write @tx_in.tx_out_id@ at COPY time, to enqueue the
    -- consumed-by pair, and to sum input values for the deposit
    -- calculation. Ingest reads the in-process 'UtxoStore'; buffered
    -- Follow checks the block-local outputs map first, because the
    -- producer's INSERT may still be unflushed.
  , resolveInputUtxo :: !(ByteString -> Word16 -> m (Maybe (TxId, TxOutId, DbLovelace)))

    -- | Record a tx's outputs so later inputs that spend them resolve
    -- in-process: into the Ingest 'UtxoStore', or into the buffered
    -- Follow resolver's block-local map.
  , recordTxOutputs :: !(ByteString -> UtxoTxEntry -> m ())

    -- | Mark the producer output consumed by the tx. Ingest buffers
    -- the pair for the 'TxOutWorker''s per-epoch bulk UPDATE; Follow
    -- runs the UPDATE inside the block's own transaction. No-op when
    -- @utxo.consumed_by_tx_id@ is off.
  , recordConsumed :: !(TxOutId -> TxId -> m ())

    -- | Drop a consumed output from the Ingest 'UtxoStore' so the
    -- store tracks the live UTxO set, not chain history. Callers pass
    -- regular inputs and phase-2 failed collateral. No-op in Follow.
  , deleteCachedUtxo :: !(ByteString -> Word16 -> m ())
  }

-- ---------------------------------------------------------------------------
-- * Accessor class
-- ---------------------------------------------------------------------------

-- | Access the ID resolver from any environment. Fixed to 'IO'
-- because every production resolver runs in 'IO'; tests can store
-- an 'IO'-backed mock.
class HasResolver env where
  getResolver :: env -> IdResolver IO
