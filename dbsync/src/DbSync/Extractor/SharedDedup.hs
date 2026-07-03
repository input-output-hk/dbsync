{-# LANGUAGE OverloadedStrings #-}

-- | Resolve-or-insert helpers for tables that are shared between
-- extractors via dedup maps.
--
-- Each helper looks up the input key in the appropriate dedup map; if
-- new, it writes the row and returns the assigned ID. Multiple
-- extractors may call the same helper for the same key — the resolver
-- guarantees only the first call produces a write.
module DbSync.Extractor.SharedDedup
  ( resolveAndWritePoolHash
  , resolveAndWriteStakeAddress
  , resolveStakeCred
  , resolveAndWriteMultiAsset
  , resolveAndWriteDatum
  , resolveAndWriteScript
  , resolveAndWriteTxScript
  , resolveAndWriteRedeemerData

    -- Governance
  , resolveAndWriteDrepHash
  , resolveAndWriteAbstractDrep
  , resolveAndWriteCommitteeHash
  , resolveAndWriteVotingAnchor
  , resolveAndWriteCostModel

  ) where

import Cardano.Prelude

import Cardano.Ledger.BaseTypes (Network (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import Cardano.Ledger.Plutus.Language (Language)
import qualified Data.ByteString as BS

import DbSync.Db.Schema.EpochBoundary (CostModel (..))
import DbSync.Db.Schema.Governance
  ( CommitteeHash (..)
  , DrepHash (..)
  , VotingAnchor (..)
  )
import DbSync.Db.Schema.Ids
  ( BlockId
  , CommitteeHashId
  , CostModelId
  , DatumId
  , DrepHashId
  , MultiAssetId
  , PoolHashId
  , RedeemerDataId
  , ScriptId
  , StakeAddressId
  , TxId
  , VotingAnchorId
  )
import DbSync.Db.Schema.MultiAsset (MultiAsset (..))
import DbSync.Db.Schema.Core (PoolHash (..), StakeAddress (..))
import DbSync.Db.Schema.ScriptsDatums (Datum, RedeemerData, Script (..))
import DbSync.Db.Types (AnchorType, VoteUrl (..))
import DbSync.Parser.Types (CredHash (..), GenericTxScript (..))
import DbSync.App.Env (HasNetwork (..))
import qualified DbSync.Extractor.EpochBoundary as EB
import DbSync.Resolver (HasResolver (..), IdResolver (..))
import DbSync.Util.Bech32
  ( mkAssetFingerprint
  , serialiseDrepToBech32
  , serialisePoolKeyHashToBech32
  , serialiseStakeKeyHashToBech32
  , serialiseStakeScriptHashToBech32
  )
import DbSync.Util.DedupHash
  ( committeeHashDedupKey
  , drepHashDedupKey
  , encodeVotingAnchorKey
  , hashDedupKey
  )
import DbSync.Writer (HasWriter (..), Writer (..))

-- ---------------------------------------------------------------------------
-- * Resolvers
-- ---------------------------------------------------------------------------

-- | Resolve a pool hash by 28-byte key hash, writing a fresh
-- @pool_hash@ row on first sighting.
resolveAndWritePoolHash
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => ByteString
  -> m PoolHashId
resolveAndWritePoolHash poolKeyHash = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  let ph = PoolHash
        { poolHashHashRaw = poolKeyHash
        , poolHashView    = serialisePoolKeyHashToBech32 poolKeyHash
        }
  (phId, isNew) <- liftIO $ resolvePoolHash resolver poolKeyHash ph
  when isNew $ liftIO $ writePoolHash writer phId ph
  pure phId

-- | Resolve a stake address by 28-byte credential hash, writing a fresh
-- @stake_address@ row on first sighting.
--
-- The stored @hash_raw@ is the full 29-byte serialised reward address
-- (header byte || credential), matching the original schema's
-- @addr29type@. The @view@ is its Bech32 encoding.
--
-- The header nibble follows CIP-19: reward addresses use @0xE_@ for a
-- stake-key credential and @0xF_@ for a stake-script credential, with
-- the low bit set on mainnet. Script credentials also populate
-- @script_hash@; key credentials leave it NULL.
resolveAndWriteStakeAddress
  :: ( HasResolver env
     , HasWriter env
     , HasNetwork env
     , MonadReader env m
     , MonadIO m
     )
  => ByteString
  -> Bool          -- ^ credential is a script hash (vs. a stake-key hash)
  -> m StakeAddressId
resolveAndWriteStakeAddress credHash isScript = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  network  <- asks getNetwork
  let mainnet  = isMainnet network
      header   = rewardAddrHeader mainnet isScript
      addr29   = BS.cons header credHash
      view     = if isScript
                   then serialiseStakeScriptHashToBech32 mainnet credHash
                   else serialiseStakeKeyHashToBech32 mainnet credHash
      sa = StakeAddress
        { stakeAddressHashRaw    = addr29
        , stakeAddressView       = view
        , stakeAddressScriptHash = if isScript then Just credHash else Nothing
        }
  -- Dedup key is the 29-byte serialised reward address — matches
  -- what 'rebuildDedupMaps' reads back from @stake_address.hash_raw@
  -- on resume.
  (saId, isNew) <- liftIO $ resolveStakeAddress resolver addr29 sa
  when isNew $ liftIO $ writeStakeAddress writer saId sa
  pure saId

-- | Resolve a 'CredHash' to its @stake_address@ id, threading the
-- script\/key flag through to the header byte and @script_hash@.
resolveStakeCred
  :: ( HasResolver env
     , HasWriter env
     , HasNetwork env
     , MonadReader env m
     , MonadIO m
     )
  => CredHash
  -> m StakeAddressId
resolveStakeCred ch = resolveAndWriteStakeAddress (chHash ch) (chIsScript ch)

-- | Reward-address header byte (CIP-19): base @0xE0@ for a stake-key
-- credential, bit 4 set (@0xF0@) for a stake-script credential, low bit
-- set on mainnet.
rewardAddrHeader :: Bool -> Bool -> Word8
rewardAddrHeader mainnet isScript =
  0xE0 .|. (if isScript then 0x10 else 0x00) .|. (if mainnet then 0x01 else 0x00)

-- | Resolve a multi-asset by @(policy, name)@, writing a fresh
-- @multi_asset@ row on first sighting.
--
-- The in-memory dedup key is @hashDedupKey (policy <> name)@. The
-- boot-time rebuild path in 'DbSync.SyncState.Row.populateMultiAsset'
-- MUST apply the same hash to the same input; otherwise resumed
-- runs will allocate fresh ids for already-known assets.
resolveAndWriteMultiAsset
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => ByteString    -- ^ policy ID
  -> ByteString    -- ^ asset name
  -> m MultiAssetId
resolveAndWriteMultiAsset policy name = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  let !key = hashDedupKey (policy <> name)
      ma = MultiAsset
        { multiAssetPolicy      = policy
        , multiAssetName        = name
        , multiAssetFingerprint = mkAssetFingerprint policy name
        }
  (maId, isNew) <- liftIO $ resolveMultiAsset resolver key ma
  when isNew $ liftIO $ writeMultiAsset writer maId ma
  pure maId

-- | Resolve a datum by 32-byte hash, writing the @datum@ row on
-- first sighting. The caller supplies the typed row so the same
-- helper covers all eras' datum shapes.
resolveAndWriteDatum
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => ByteString
  -> Datum
  -> m DatumId
resolveAndWriteDatum hash row = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  (did, isNew) <- liftIO $ resolveDatum resolver hash row
  when isNew $ liftIO $ writeDatum writer did row
  pure did

-- | Resolve a script by its hash, writing the @script@ row on
-- first sighting.
resolveAndWriteScript
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => ByteString
  -> Script
  -> m ScriptId
resolveAndWriteScript hash row = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  (sid, isNew) <- liftIO $ resolveScript resolver hash row
  when isNew $ liftIO $ writeScript writer sid row
  pure sid

-- | Resolve a parsed script (witness, aux-data, or output reference)
-- by hash, writing the @script@ row — attributed to the carrying tx —
-- on first sighting.
resolveAndWriteTxScript
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => TxId
  -> GenericTxScript
  -> m ScriptId
resolveAndWriteTxScript txId gts =
  resolveAndWriteScript (gtsHash gts) Script
    { scriptTxId           = txId
    , scriptHash           = gtsHash gts
    , scriptType           = gtsType gts
    , scriptJson           = gtsJson gts
    , scriptBytes          = gtsBytes gts
    , scriptSerialisedSize = gtsSerialisedSize gts
    }

-- | Resolve a redeemer-data payload by 32-byte hash, writing the
-- @redeemer_data@ row on first sighting.
resolveAndWriteRedeemerData
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => ByteString
  -> RedeemerData
  -> m RedeemerDataId
resolveAndWriteRedeemerData hash row = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  (rdid, isNew) <- liftIO $ resolveRedeemerData resolver hash row
  when isNew $ liftIO $ writeRedeemerData writer rdid row
  pure rdid

-- ---------------------------------------------------------------------------
-- * Governance dedup helpers
-- ---------------------------------------------------------------------------

-- | Resolve a concrete DRep credential, writing the @drep_hash@ row
-- on first sighting. @has_script@ flags script-credential DReps.
resolveAndWriteDrepHash
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => ByteString    -- ^ 28-byte credential hash
  -> Bool          -- ^ @has_script@
  -> m DrepHashId
resolveAndWriteDrepHash credHash hasScript = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  let row = DrepHash
        { drepHashRaw       = Just credHash
        , drepHashView      = serialiseDrepToBech32 credHash
        , drepHashHasScript = hasScript
        }
  (did, isNew) <- liftIO $ resolveDrepHash resolver (drepHashDedupKey (Just credHash) (drepHashView row)) row
  when isNew $ liftIO $ writeDrepHash writer did row
  pure did

-- | Resolve one of the two abstract DReps (@always_abstain@,
-- @always_no_confidence@). The @raw@ column is NULL on these rows;
-- the @view@ string is the dedup key.
resolveAndWriteAbstractDrep
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => Text          -- ^ Sentinel @view@ string.
  -> m DrepHashId
resolveAndWriteAbstractDrep viewText = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  let row = DrepHash
        { drepHashRaw       = Nothing
        , drepHashView      = viewText
        , drepHashHasScript = False
        }
  (did, isNew) <- liftIO $ resolveDrepHash resolver (drepHashDedupKey Nothing viewText) row
  when isNew $ liftIO $ writeDrepHash writer did row
  pure did

-- | Resolve a committee-key credential by @(raw, has_script)@,
-- writing the @committee_hash@ row on first sighting.
resolveAndWriteCommitteeHash
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => ByteString    -- ^ 28-byte credential hash
  -> Bool          -- ^ @has_script@
  -> m CommitteeHashId
resolveAndWriteCommitteeHash credHash hasScript = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  let row = CommitteeHash
        { committeeHashRaw       = credHash
        , committeeHashHasScript = hasScript
        }
  (cid, isNew) <- liftIO $ resolveCommitteeHash resolver (committeeHashDedupKey credHash hasScript) row
  when isNew $ liftIO $ writeCommitteeHash writer cid row
  pure cid

-- | Resolve a voting anchor by @(url, data_hash, type)@, writing
-- the @voting_anchor@ row on first sighting.
resolveAndWriteVotingAnchor
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => Text          -- ^ Anchor URL.
  -> ByteString    -- ^ 32-byte data hash.
  -> AnchorType
  -> BlockId       -- ^ Block in which the anchor was first observed.
  -> m VotingAnchorId
resolveAndWriteVotingAnchor url dataHash anchorType blockId = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  let !key = encodeVotingAnchorKey url dataHash anchorType
      row = VotingAnchor
        { votingAnchorUrl      = VoteUrl url
        , votingAnchorDataHash = dataHash
        , votingAnchorType     = anchorType
        , votingAnchorBlockId  = blockId
        }
  (vid, isNew) <- liftIO $ resolveVotingAnchor resolver key anchorType row
  when isNew $ liftIO $ writeVotingAnchor writer vid row
  pure vid

-- | Resolve a Plutus cost-model map by its canonical CBOR hash,
-- writing the @cost_model@ row on first sighting. Lives here rather
-- than in 'Extractor.EpochBoundary' so both the epoch-boundary path
-- and the governance @ParameterChange@ path can share the dedup
-- cache.
resolveAndWriteCostModel
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => Map Language Alonzo.CostModel
  -> m CostModelId
resolveAndWriteCostModel cms = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  let row = EB.mkCostModelRow cms
  (cmId, isNew) <- liftIO $ resolveCostModel resolver (costModelHash row) row
  when isNew $ liftIO $ writeCostModel writer cmId row
  pure cmId

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

isMainnet :: Network -> Bool
isMainnet Mainnet = True
isMainnet Testnet = False
