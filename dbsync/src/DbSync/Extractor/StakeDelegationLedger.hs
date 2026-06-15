{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Ledger-derived stake-delegation extractor.
--
-- Owns four tables:
--
--   * @epoch_stake@ — per-(stake, pool, epoch) active stake, emitted
--     from per-block slices of the ledger's "mark" snapshot.
--   * @epoch_stake_progress@ — one row per completed epoch's slicing.
--   * @reward@ — per-block-production rewards (leader / member) and
--     pool-deposit refunds. Boundary-only, sourced from @apEvents@.
--   * @pot_reward@ — pot-sourced payouts: MIR distributions
--     (Shelley→Babbage) and Conway+ enacted treasury withdrawals.
--     Boundary-only.
--
-- All four tables are IDENTITY leaves; PostgreSQL allocates ids.
--
-- Known omissions:
--
--   * 'LedgerRestrainedRewards' — the original cardano-db-sync DELETEs
--     reward rows for stake-deregistered credentials; we leave the
--     rows in place. Tracked as a known divergence.
--   * 'LedgerGovInfo' deposit refunds (the @garReturnAddr@ / @garDeposit@
--     half) are not written; only enacted treasury withdrawals land
--     in @pot_reward@.
module DbSync.Extractor.StakeDelegationLedger
  ( stakeDelegationLedgerExtractor
  , runStakeDelegationLedgerBoundary
  ) where

import Cardano.Prelude

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Rewards as Ledger
import Cardano.Slotting.Slot (EpochNo (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict

import DbSync.Db.Schema.Ids (BlockId)
import DbSync.Db.Schema.StakeDelegation
  ( EpochStake (..)
  , EpochStakeProgress (..)
  , PotReward (..)
  , Reward (..)
  , epochStakeProgressTableDef
  , epochStakeTableDef
  , potRewardTableDef
  , rewardTableDef
  )
import DbSync.Db.Types (DbLovelace (..))
import DbSync.Extractor
  ( BlockContext (..)
  , ExtractorDef (..)
  , HasNetwork
  , ProcessBlockFn
  , blockStakeSlice
  )
import DbSync.Extractor.SharedDedup
  ( resolveAndWritePoolHash
  , resolveAndWriteStakeAddress
  )
import DbSync.Resolver (HasResolver)
import DbSync.Util (rewardAddrCred)
import qualified DbSync.Worker.Ledger.EpochUpdate as Generic
import DbSync.Worker.Ledger.Event
  ( GovActionRefunded (..)
  , LedgerEvent (..)
  )
import DbSync.Worker.Ledger.Keys (PoolKeyHash, StakeCred)
import qualified DbSync.Worker.Ledger.Rewards as Generic
import DbSync.Worker.Ledger.Rewards
  ( RewardSource (..)
  , rewardTypeToSource
  )
import DbSync.Worker.Ledger.StakeDist
  ( StakeSlice (..)
  , StakeSliceRes (..)
  )
import DbSync.Worker.Ledger.Types (ApplyResult (..))
import DbSync.Writer (HasWriter (..), Writer (..))

-- ---------------------------------------------------------------------------
-- * Extractor registration
-- ---------------------------------------------------------------------------

stakeDelegationLedgerExtractor :: ExtractorDef
stakeDelegationLedgerExtractor = ExtractorDef
  { pdName    = "stake_delegation_ledger"
  , pdTables  =
      [ rewardTableDef
      , potRewardTableDef
      , epochStakeTableDef
      , epochStakeProgressTableDef
      ]
  , pdProcess = processStakeDelegationLedger
  }

-- ---------------------------------------------------------------------------
-- * Per-block processing
-- ---------------------------------------------------------------------------

-- | Drain this block's stake slice into @epoch_stake@ rows. On the
-- last slice of an epoch, also emit one @epoch_stake_progress@ row.
-- No-op for Byron and pre-Shelley blocks (which produce 'NoSlices').
processStakeDelegationLedger :: ProcessBlockFn
processStakeDelegationLedger ctx =
  case blockStakeSlice (bcLedgerData ctx) of
    NoSlices            -> pure ()
    Slice slice isLast -> emitSlice slice isLast

emitSlice
  :: ( HasResolver env
     , HasWriter env
     , HasNetwork env
     , MonadReader env m
     , MonadIO m
     )
  => StakeSlice -> Bool -> m ()
emitSlice slice isLast = do
  writer <- asks getWriter
  let epoch = unEpochNo (sliceEpochNo slice)
  forM_ (sliceDistr slice) $ \(stakeCred, (Coin amount, poolKey)) -> do
    saId      <- resolveAndWriteStakeAddress (stakeCredBytes stakeCred)
    (phId, _) <- resolveAndWritePoolHash (poolKeyHashBytes poolKey)
    liftIO $ writeEpochStake writer EpochStake
      { epochStakeAddrId  = saId
      , epochStakePoolId  = phId
      , epochStakeAmount  = DbLovelace (fromIntegral amount)
      , epochStakeEpochNo = epoch
      }
  when isLast $
    liftIO $ writeEpochStakeProgress writer EpochStakeProgress
      { epochStakeProgressEpochNo   = epoch
      , epochStakeProgressCompleted = True
      }

-- ---------------------------------------------------------------------------
-- * Boundary handler
-- ---------------------------------------------------------------------------

-- | Iterate @apEvents@ and emit @reward@ + @pot_reward@ rows.
--
-- No-op when 'apNewEpoch' is 'Strict.Nothing' (mid-epoch block). The
-- @BlockId@ argument is unused; carried for signature symmetry with
-- the other boundary handlers.
runStakeDelegationLedgerBoundary
  :: ( HasResolver env
     , HasWriter env
     , HasNetwork env
     , MonadReader env m
     , MonadIO m
     )
  => ApplyResult -> BlockId -> m ()
runStakeDelegationLedgerBoundary applyResult _blockId =
  case apNewEpoch applyResult of
    Strict.Nothing       -> pure ()
    Strict.Just newEpoch ->
      forM_ (apEvents applyResult)
        (processEvent (unEpochNo (Generic.neEpoch newEpoch)))

processEvent
  :: ( HasResolver env
     , HasWriter env
     , HasNetwork env
     , MonadReader env m
     , MonadIO m
     )
  => Word64 -> LedgerEvent -> m ()
processEvent currentEpoch = \case
  LedgerTotalRewards e rewardsMap ->
    emitLedgerRewards (unEpochNo e) rewardsMap
  LedgerPoolReap e (Generic.Rewards rewardsMap) ->
    emitGenericRewards (unEpochNo e) rewardsMap
  LedgerMirDist potMap ->
    emitMirDist (currentEpoch + 1) potMap
  LedgerGovInfo enacted _dropped _expired _uncl ->
    emitGovInfoTreasury (currentEpoch + 1) enacted
  -- LedgerRestrainedRewards: the original cardano-db-sync DELETEs
  -- @reward@ rows for stake-deregistered credentials. We skip the
  -- event; documented as a known divergence on the module.
  LedgerRestrainedRewards {} -> pure ()
  _ -> pure ()

-- | Per-pool rewards from 'LedgerTotalRewards'. The cardano-ledger
-- shape splits by 'Ledger.RewardType' (leader vs member); we map
-- through 'rewardTypeToSource' to the DB enum.
emitLedgerRewards
  :: ( HasResolver env
     , HasWriter env
     , HasNetwork env
     , MonadReader env m
     , MonadIO m
     )
  => Word64
  -> Map StakeCred (Set Ledger.Reward)
  -> m ()
emitLedgerRewards spendable rewardsMap = do
  writer <- asks getWriter
  forM_ (Map.toList rewardsMap) $ \(stakeCred, rewardSet) -> do
    saId <- resolveAndWriteStakeAddress (stakeCredBytes stakeCred)
    forM_ (Set.toList rewardSet) $ \r -> do
      (phId, _) <- resolveAndWritePoolHash (poolKeyHashBytes (Ledger.rewardPool r))
      liftIO $ writeReward writer Reward
        { rewardAddrId         = saId
        , rewardType           = rewardTypeToSource (Ledger.rewardType r)
        , rewardAmount         = DbLovelace (fromIntegral (unCoin (Ledger.rewardAmount r)))
        , rewardSpendableEpoch = spendable
        , rewardPoolId         = phId
        , rewardEarnedEpoch    = 0      -- PG fills via the generated column
        }

-- | Pool-deposit refunds from 'LedgerPoolReap' arrive pre-converted
-- into our 'Generic.Reward' shape, so no 'rewardTypeToSource' bounce
-- is needed.
emitGenericRewards
  :: ( HasResolver env
     , HasWriter env
     , HasNetwork env
     , MonadReader env m
     , MonadIO m
     )
  => Word64
  -> Map StakeCred (Set Generic.Reward)
  -> m ()
emitGenericRewards spendable rewardsMap = do
  writer <- asks getWriter
  forM_ (Map.toList rewardsMap) $ \(stakeCred, rewardSet) -> do
    saId <- resolveAndWriteStakeAddress (stakeCredBytes stakeCred)
    forM_ (Set.toList rewardSet) $ \r -> do
      (phId, _) <- resolveAndWritePoolHash (poolKeyHashBytes (Generic.rewardPool r))
      liftIO $ writeReward writer Reward
        { rewardAddrId         = saId
        , rewardType           = Generic.rewardSource r
        , rewardAmount         = DbLovelace (Generic.rewardAmount r)
        , rewardSpendableEpoch = spendable
        , rewardPoolId         = phId
        , rewardEarnedEpoch    = 0
        }

-- | MIR distribution (reserves / treasury). Pre-Conway only;
-- Conway+ produces zero entries because the MIR rule was removed.
emitMirDist
  :: ( HasResolver env
     , HasWriter env
     , HasNetwork env
     , MonadReader env m
     , MonadIO m
     )
  => Word64
  -> Map StakeCred (Set Generic.PotReward)
  -> m ()
emitMirDist spendable potMap = do
  writer <- asks getWriter
  forM_ (Map.toList potMap) $ \(stakeCred, potSet) -> do
    saId <- resolveAndWriteStakeAddress (stakeCredBytes stakeCred)
    forM_ (Set.toList potSet) $ \pr ->
      liftIO $ writePotReward writer PotReward
        { potRewardAddrId         = saId
        , potRewardType           = Generic.prSource pr
        , potRewardAmount         = DbLovelace (fromIntegral (unCoin (Generic.prAmount pr)))
        , potRewardSpendableEpoch = spendable
        , potRewardEarnedEpoch    = 0      -- PG fills via the generated column
        }

-- | Conway enacted treasury withdrawals. Each withdrawal pays a
-- 'Coin' to an 'AccountAddress'; we credit the matching
-- @stake_address@ with a 'RwdTreasury'-tagged @pot_reward@ row.
emitGovInfoTreasury
  :: ( HasResolver env
     , HasWriter env
     , HasNetwork env
     , MonadReader env m
     , MonadIO m
     )
  => Word64
  -> [GovActionRefunded]
  -> m ()
emitGovInfoTreasury spendable enacted = do
  writer <- asks getWriter
  forM_ enacted $ \refunded ->
    case garMTreasury refunded of
      Nothing       -> pure ()
      Just payments ->
        forM_ (Map.toList payments) $ \(acct, coin) -> do
          saId <- resolveAndWriteStakeAddress (accountAddrCred acct)
          liftIO $ writePotReward writer PotReward
            { potRewardAddrId         = saId
            , potRewardType           = RwdTreasury
            , potRewardAmount         = DbLovelace (fromIntegral (unCoin coin))
            , potRewardSpendableEpoch = spendable
            , potRewardEarnedEpoch    = 0
            }

-- ---------------------------------------------------------------------------
-- * Key extraction helpers
-- ---------------------------------------------------------------------------

stakeCredBytes :: StakeCred -> ByteString
stakeCredBytes = \case
  Ledger.KeyHashObj (Ledger.KeyHash h)      -> Crypto.hashToBytes h
  Ledger.ScriptHashObj (Core.ScriptHash h)  -> Crypto.hashToBytes h

poolKeyHashBytes :: PoolKeyHash -> ByteString
poolKeyHashBytes (Ledger.KeyHash h) = Crypto.hashToBytes h

accountAddrCred :: Ledger.AccountAddress -> ByteString
accountAddrCred = rewardAddrCred . Ledger.serialiseAccountAddress
