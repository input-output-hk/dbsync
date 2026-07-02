{-# LANGUAGE OverloadedStrings #-}

-- | Per-epoch pool distribution projection.
--
-- Owns the @pool_stat@ table. One row per (pool, epoch); written
-- from the post-epoch ledger state at each boundary crossing. The
-- per-block 'pdProcess' callback is a no-op; 'runPoolStatsBoundary'
-- is invoked by the consumer when an epoch crosses and the
-- LedgerWorker has produced the matching 'ApplyResult'.
--
-- When the ledger feature is off the consumer never calls
-- 'runPoolStatsBoundary' and the table stays empty.
--
-- @voting_power@ is written as 0 across all eras; the post-CIP-145
-- derivation is not yet wired.
module DbSync.Extractor.PoolStats
  ( poolStatsExtractor
  , runPoolStatsBoundary
  ) where

import Cardano.Prelude

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Slotting.Slot (EpochNo (..))
import qualified Data.Map.Strict as Map
import qualified Data.Strict.Maybe as Strict

import qualified DbSync.Worker.Ledger.EpochUpdate as Generic
import DbSync.Db.Schema.Ids (BlockId)
import DbSync.Db.Schema.Pool (PoolStat (..), poolStatTableDef)
import DbSync.Db.Types (DbWord64 (..))
import DbSync.Extractor (ExtractorDef (..))
import DbSync.Extractor.SharedDedup (resolveAndWritePoolHash)
import DbSync.Resolver (HasResolver)
import DbSync.Worker.Ledger.Keys (PoolKeyHash)
import DbSync.Worker.Ledger.Types (BoundaryApplyData (..))
import DbSync.Writer (HasWriter (..), Writer (..))

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.Keys as Ledger

-- ---------------------------------------------------------------------------
-- * Extractor registration
-- ---------------------------------------------------------------------------

poolStatsExtractor :: ExtractorDef
poolStatsExtractor = ExtractorDef
  { pdName    = "pool_stats"
  , pdTables  = [poolStatTableDef]
  , pdProcess = \_ -> pure ()
  }

-- ---------------------------------------------------------------------------
-- * Boundary handler
-- ---------------------------------------------------------------------------

-- | Emit one @pool_stat@ row per pool in the post-epoch stake
-- distribution. No-op when 'bndNewEpoch' is 'Strict.Nothing'
-- (mid-epoch block) or 'nePoolDistr' is 'Strict.Nothing' (Byron
-- boundary).
--
-- @BlockId@ is unused; carried for signature symmetry with the other
-- boundary handlers.
runPoolStatsBoundary
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => BoundaryApplyData -> BlockId -> m ()
runPoolStatsBoundary applyResult _blockId =
  case bndNewEpoch applyResult of
    Strict.Nothing       -> pure ()
    Strict.Just newEpoch -> case Generic.nePoolDistr newEpoch of
      Strict.Nothing                       -> pure ()
      Strict.Just (stakePerPool, blocksPerPool) -> do
        let epoch = unEpochNo (Generic.neEpoch newEpoch)
        for_ (Map.toList stakePerPool) $ \(poolKey, (stake, delegators)) ->
          writePoolStatRow epoch blocksPerPool poolKey stake delegators

-- | Build and dispatch a single @pool_stat@ row. Resolves the pool
-- hash via the shared dedup helper so the row's FK points at the
-- existing @pool_hash@ row.
writePoolStatRow
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => Word64
  -> Map PoolKeyHash Natural
  -> PoolKeyHash
  -> Coin
  -> Word64
  -> m ()
writePoolStatRow epoch blocksPerPool poolKey stake delegators = do
  writer <- asks getWriter
  phId <- resolveAndWritePoolHash (poolKeyHashBytes poolKey)
  let blocks = fromMaybe 0 (Map.lookup poolKey blocksPerPool)
  liftIO $ writePoolStat writer PoolStat
    { poolStatPoolHashId         = phId
    , poolStatEpochNo            = epoch
    , poolStatNumberOfBlocks     = DbWord64 (fromIntegral blocks)
    , poolStatNumberOfDelegators = DbWord64 delegators
    , poolStatStake              = DbWord64 (fromIntegral (unCoin stake))
    , poolStatVotingPower        = Just (DbWord64 0)
    }

-- | Extract the raw 28-byte hash from a 'PoolKeyHash' newtype.
poolKeyHashBytes :: PoolKeyHash -> ByteString
poolKeyHashBytes (Ledger.KeyHash h) = Crypto.hashToBytes h
