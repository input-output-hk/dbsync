{-# LANGUAGE OverloadedStrings #-}

-- | Per-epoch pool distribution projection.
--
-- Owns the @pool_stat@ table. One row per (pool, epoch); written
-- from the post-epoch ledger state at each boundary crossing. The
-- row set is the union of pools with active stake, pools that made
-- blocks in the previous epoch, and pools in the DRep snapshot's SPO
-- voting distribution — absent entries fill with zeros so
-- registered-but-inactive pools still get a row.
--
-- The per-block 'pdProcess' callback is a no-op; 'runPoolStatsBoundary'
-- is invoked by the consumer when an epoch crosses and the
-- LedgerWorker has produced the matching 'ApplyResult'.
--
-- When the ledger feature is off the consumer never calls
-- 'runPoolStatsBoundary' and the table stays empty.
module DbSync.Extractor.PoolStats
  ( poolStatsExtractor
  , runPoolStatsBoundary
  ) where

import Cardano.Prelude

import Cardano.Ledger.Coin (Coin (..), CompactForm)
import Cardano.Slotting.Slot (EpochNo (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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
import qualified Cardano.Ledger.Compactible as Ledger
import qualified Cardano.Ledger.Conway.Governance as Gov
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

-- | Emit one @pool_stat@ row per pool at an epoch crossing. No-op
-- when 'bndNewEpoch' is 'Strict.Nothing' (mid-epoch block) or
-- 'nePoolDistr' is 'Strict.Nothing' (Byron boundary).
--
-- @voting_power@ comes from the DRep snapshot's SPO distribution and
-- is only derivable when the governance extractor is enabled (the
-- snapshot rides 'Generic.neDRepState', Conway+); otherwise the
-- column is NULL.
--
-- @BlockId@ is unused; carried for signature symmetry with the other
-- boundary handlers.
runPoolStatsBoundary
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => Bool  -- ^ governance extractor enabled
  -> BoundaryApplyData -> BlockId -> m ()
runPoolStatsBoundary governanceOn applyResult _blockId =
  case bndNewEpoch applyResult of
    Strict.Nothing       -> pure ()
    Strict.Just newEpoch -> case Generic.nePoolDistr newEpoch of
      Strict.Nothing                       -> pure ()
      Strict.Just (stakePerPool, blocksPerPool) -> do
        let epoch = unEpochNo (Generic.neEpoch newEpoch)
            spoVoting
              | governanceOn
              , Strict.Just dreps <- Generic.neDRepState newEpoch =
                  Gov.psPoolDistr (fst (Gov.finishDRepPulser dreps))
              | otherwise = Map.empty
            allPools = Map.keysSet stakePerPool
                    <> Map.keysSet blocksPerPool
                    <> Map.keysSet spoVoting
        for_ (Set.toList allPools) $ \poolKey ->
          writePoolStatRow epoch stakePerPool blocksPerPool spoVoting poolKey

-- | Build and dispatch a single @pool_stat@ row, zero-filling the
-- maps the pool is absent from. Resolves the pool hash via the
-- shared dedup helper so the row's FK points at the existing
-- @pool_hash@ row.
writePoolStatRow
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => Word64
  -> Map PoolKeyHash (Coin, Word64)
  -> Map PoolKeyHash Natural
  -> Map PoolKeyHash (CompactForm Coin)
  -> PoolKeyHash
  -> m ()
writePoolStatRow epoch stakePerPool blocksPerPool spoVoting poolKey = do
  writer <- asks getWriter
  phId <- resolveAndWritePoolHash (poolKeyHashBytes poolKey)
  let (stake, delegators) = fromMaybe (Coin 0, 0) (Map.lookup poolKey stakePerPool)
      blocks = fromMaybe 0 (Map.lookup poolKey blocksPerPool)
      votingPower = DbWord64 . fromIntegral . unCoin . Ledger.fromCompact
        <$> Map.lookup poolKey spoVoting
  liftIO $ writePoolStat writer PoolStat
    { poolStatPoolHashId         = phId
    , poolStatEpochNo            = epoch
    , poolStatNumberOfBlocks     = DbWord64 (fromIntegral blocks)
    , poolStatNumberOfDelegators = DbWord64 delegators
    , poolStatStake              = DbWord64 (fromIntegral (unCoin stake))
    , poolStatVotingPower        = votingPower
    }

-- | Extract the raw 28-byte hash from a 'PoolKeyHash' newtype.
poolKeyHashBytes :: PoolKeyHash -> ByteString
poolKeyHashBytes (Ledger.KeyHash h) = Crypto.hashToBytes h
