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
  , resolveAndWriteMultiAsset
  ) where

import Cardano.Prelude

import Cardano.Ledger.BaseTypes (Network (..))
import qualified Data.ByteString as BS

import DbSync.Db.Schema.Ids (MultiAssetId, PoolHashId, StakeAddressId)
import DbSync.Db.Schema.MultiAsset (MultiAsset (..))
import DbSync.Db.Schema.Pool (PoolHash (..))
import DbSync.Db.Schema.StakeDelegation (StakeAddress (..))
import DbSync.Env (HasNetwork (..))
import DbSync.Resolver (HasResolver (..), IdResolver (..))
import DbSync.Util.Bech32
  ( mkAssetFingerprint
  , serialisePoolKeyHashToBech32
  , serialiseStakeKeyHashToBech32
  )
import DbSync.Util.DedupHash (hashDedupKey)
import DbSync.Writer (HasWriter (..), Writer (..))

-- ---------------------------------------------------------------------------
-- * Resolvers
-- ---------------------------------------------------------------------------

-- | Resolve a pool hash by 28-byte key hash, writing a fresh
-- @pool_hash@ row on first sighting.
--
-- The 'Bool' is 'True' when this call assigned a new ID — callers
-- that need to distinguish first registration from re-registration
-- (e.g. @pool_update.active_epoch_no@: +2 vs +3 epoch offset) consult
-- the flag instead of querying the DB.
resolveAndWritePoolHash
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => ByteString
  -> m (PoolHashId, Bool)
resolveAndWritePoolHash poolKeyHash = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  let ph = PoolHash
        { poolHashHashRaw = poolKeyHash
        , poolHashView    = serialisePoolKeyHashToBech32 poolKeyHash
        }
  result@(phId, isNew) <- liftIO $ resolvePoolHash resolver poolKeyHash ph
  when isNew $ liftIO $ writePoolHash writer phId ph
  pure result

-- | Resolve a stake address by 28-byte credential hash, writing a fresh
-- @stake_address@ row on first sighting.
--
-- The stored @hash_raw@ is the full 29-byte serialised reward address
-- (header byte || credential), matching the original schema's
-- @addr29type@. The @view@ is its Bech32 encoding.
--
-- Script-hash detection is deferred — the row currently always has
-- @script_hash = Nothing@ and uses the stake-key (not stake-script)
-- header.
resolveAndWriteStakeAddress
  :: ( HasResolver env
     , HasWriter env
     , HasNetwork env
     , MonadReader env m
     , MonadIO m
     )
  => ByteString
  -> m StakeAddressId
resolveAndWriteStakeAddress credHash = do
  resolver <- asks getResolver
  writer   <- asks getWriter
  network  <- asks getNetwork
  let mainnet = isMainnet network
      header  = if mainnet then 0xE1 else 0xE0
      addr29  = BS.cons header credHash
      sa = StakeAddress
        { stakeAddressHashRaw    = addr29
        , stakeAddressView       = serialiseStakeKeyHashToBech32 mainnet credHash
        , stakeAddressScriptHash = Nothing
        }
  -- Dedup key is the 29-byte serialised reward address — matches
  -- what 'rebuildDedupMaps' reads back from @stake_address.hash_raw@
  -- on resume.
  (saId, isNew) <- liftIO $ resolveStakeAddress resolver addr29 sa
  when isNew $ liftIO $ writeStakeAddress writer saId sa
  pure saId

-- | Resolve a multi-asset by @(policy, name)@, writing a fresh
-- @multi_asset@ row on first sighting.
--
-- The in-memory dedup key is @hashDedupKey (policy <> name)@. The
-- boot-time rebuild path in 'DbSync.Checkpoint.SyncState.populateMultiAsset'
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

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

isMainnet :: Network -> Bool
isMainnet Mainnet = True
isMainnet Testnet = False
