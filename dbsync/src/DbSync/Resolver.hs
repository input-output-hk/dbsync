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
import DbSync.Db.Schema.Ids
import DbSync.Db.Schema.MultiAsset (MultiAsset)
import DbSync.Db.Schema.Pool (PoolHash)
import DbSync.Db.Schema.StakeDelegation (StakeAddress)
import DbSync.Db.Types (DbLovelace)
import DbSync.Phase.Ingest.UtxoStore (UtxoTxEntry)

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

    -- | Assign the next tx_in ID.
  , assignTxInId           :: !(m TxInId)

    -- | Assign the next collateral_tx_in ID.
  , assignCollateralTxInId :: !(m CollateralTxInId)
  , assignCollateralTxOutId :: !(m CollateralTxOutId)

    -- | Assign the next reference_tx_in ID.
  , assignReferenceTxInId  :: !(m ReferenceTxInId)

    -- ---------------------------------------------------------------
    -- Metadata extractor IDs
    -- ---------------------------------------------------------------

    -- | Assign the next tx_metadata ID.
  , assignTxMetadataId :: !(m TxMetadataId)

    -- ---------------------------------------------------------------
    -- MultiAsset extractor IDs
    -- ---------------------------------------------------------------

    -- | Resolve a multi-asset by its (policy ++ name) key.
    -- Key is 'ShortByteString' (unpinned) to avoid pinned ByteString
    -- concatenation in the hot multi-asset lookup path.
    -- Returns @(MultiAssetId, isNew)@.
  , resolveMultiAsset :: !(ShortByteString -> MultiAsset -> m (MultiAssetId, Bool))

    -- | Assign the next ma_tx_mint ID.
  , assignMaTxMintId :: !(m MaTxMintId)

    -- | Assign the next ma_tx_out ID.
  , assignMaTxOutId  :: !(m MaTxOutId)

    -- ---------------------------------------------------------------
    -- StakeDelegation extractor IDs
    -- ---------------------------------------------------------------

    -- | Resolve a stake address by its credential hash.
    -- Returns @(StakeAddressId, isNew)@.
  , resolveStakeAddress :: !(ByteString -> StakeAddress -> m (StakeAddressId, Bool))

    -- | Assign the next stake_registration ID.
  , assignStakeRegistrationId :: !(m StakeRegistrationId)

    -- | Assign the next stake_deregistration ID.
  , assignStakeDeregistrationId :: !(m StakeDeregistrationId)

    -- | Assign the next delegation ID.
  , assignDelegationId :: !(m DelegationId)

    -- | Assign the next withdrawal ID.
  , assignWithdrawalId :: !(m WithdrawalId)

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

    -- | Assign the next pool_owner ID.
  , assignPoolOwnerId :: !(m PoolOwnerId)

    -- | Assign the next pool_retire ID.
  , assignPoolRetireId :: !(m PoolRetireId)

    -- | Assign the next pool_relay ID.
  , assignPoolRelayId :: !(m PoolRelayId)

    -- ---------------------------------------------------------------
    -- CBOR extractor IDs
    -- ---------------------------------------------------------------

    -- | Assign the next tx_cbor ID.
  , assignTxCborId :: !(m TxCborId)

    -- ---------------------------------------------------------------
    -- EpochSyncStats IDs
    -- ---------------------------------------------------------------

    -- | Assign the next epoch_sync_stats ID.
  , assignEpochSyncStatsId :: !(m EpochSyncStatsId)

    -- ---------------------------------------------------------------
    -- EpochBoundary IDs
    -- ---------------------------------------------------------------

    -- | Assign the next ada_pots ID.
  , assignAdaPotsId :: !(m AdaPotsId)

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
