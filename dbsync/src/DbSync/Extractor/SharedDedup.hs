{-# LANGUAGE OverloadedStrings #-}

-- | Resolve-or-insert helpers for the tables that extractors share
-- through dedup maps. Each helper looks the key up, then writes the
-- row only when the key is new. Several extractors may call the same
-- helper with the same key; the resolver lets only the first call
-- write.
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

-- | The key is the 28-byte pool key hash.
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

-- | The key is the 28-byte credential hash. The stored @hash_raw@ is
-- the full 29-byte serialised reward address (@header || credential@),
-- which the @addr29type@ domain holds, and @view@ is its Bech32 form.
--
-- The header nibble follows CIP-19: a reward address uses @0xE_@ for a
-- stake-key credential and @0xF_@ for a stake-script credential, with
-- the low bit set on mainnet. A script credential also fills
-- @script_hash@; a key credential leaves it NULL.
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

-- | The credential's script\/key flag drives both the header byte and
-- @script_hash@.
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

-- | The in-memory dedup key is @hashDedupKey (policy <> name)@. The
-- boot-time rebuild in 'DbSync.SyncState.Row.populateMultiAsset' MUST
-- apply the same hash to the same input. If it does not, a resumed run
-- allocates fresh ids for assets the database already holds.
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

-- | The caller supplies the typed row, so this one helper covers the
-- datum shape of every era.
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

-- | Takes a parsed script from a witness set, from auxiliary data, or
-- from an output reference, and attributes the row to the tx that
-- carries it.
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

-- | Handles the concrete DRep credentials.
-- 'resolveAndWriteAbstractDrep' handles the other two.
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

-- | Handles the two abstract DReps, @always_abstain@ and
-- @always_no_confidence@. Their @raw@ column is NULL, so the @view@
-- string is the dedup key.
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

-- | The dedup key is the @(raw, has_script)@ pair.
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

-- | The dedup key is the @(url, data_hash, type)@ triple.
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

-- | The dedup key is the canonical CBOR hash of the cost-model map.
-- This helper lives here, not in 'Extractor.EpochBoundary', so the
-- epoch-boundary path and the governance @ParameterChange@ path share
-- one dedup cache.
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
