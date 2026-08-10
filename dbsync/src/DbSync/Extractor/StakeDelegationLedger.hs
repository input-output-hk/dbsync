{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Ledger-derived stake-delegation extractor. It slices the ledger's
-- "mark" stake snapshot into @epoch_stake@ and @epoch_stake_progress@
-- per block, and fills @reward@ and @pot_reward@ from boundary events.
-- All four tables are IDENTITY leaves, so PostgreSQL allocates the ids.
--
-- Two known omissions: 'LedgerRestrainedRewards' does not delete the
-- reward rows of stake-deregistered credentials, and 'LedgerGovInfo'
-- deposit refunds (the @garReturnAddr@ and @garDeposit@ half) reach no
-- table. Only enacted treasury withdrawals land in @pot_reward@.
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
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict

import DbSync.Db.Schema.Ids (BlockId, PoolHashId)
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
  , resolveStakeCred
  )
import DbSync.Parser.Types (CredHash (..), rewardAddrCredHash)
import DbSync.Resolver (HasResolver)
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
import DbSync.Worker.Ledger.Types (BoundaryApplyData (..))
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
    saId      <- resolveStakeCred (stakeCredHash stakeCred)
    phId <- resolveAndWritePoolHash (poolKeyHashBytes poolKey)
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

-- | Drain the ended epoch's catch-up stake slice, then iterate
-- @bndEvents@ and emit @reward@ + @pot_reward@ rows.
--
-- The @BlockId@ argument is unused; carried for signature symmetry
-- with the other boundary handlers.
runStakeDelegationLedgerBoundary
  :: ( HasResolver env
     , HasWriter env
     , HasNetwork env
     , MonadReader env m
     , MonadIO m
     )
  => BoundaryApplyData -> BlockId -> m ()
runStakeDelegationLedgerBoundary applyResult _blockId = do
  case bndCatchupStakeSlice applyResult of
    NoSlices           -> pure ()
    Slice slice isLast -> emitSlice slice isLast
  case bndNewEpoch applyResult of
    Strict.Nothing       -> pure ()
    Strict.Just newEpoch -> do
      poolCache <- liftIO $ newIORef Map.empty
      forM_ (bndEvents applyResult)
        (processEvent poolCache (unEpochNo (Generic.neEpoch newEpoch)))

processEvent
  :: ( HasResolver env
     , HasWriter env
     , HasNetwork env
     , MonadReader env m
     , MonadIO m
     )
  => IORef (Map ByteString PoolHashId) -> Word64 -> LedgerEvent -> m ()
processEvent poolCache currentEpoch = \case
  LedgerTotalRewards e rewardsMap ->
    emitLedgerRewards poolCache (unEpochNo e) rewardsMap
  LedgerPoolReap e (Generic.Rewards rewardsMap) ->
    emitGenericRewards poolCache (unEpochNo e) rewardsMap
  LedgerMirDist potMap ->
    emitMirDist (currentEpoch + 1) potMap
  LedgerGovInfo enacted _dropped _expired _uncl ->
    emitGovInfoTreasury (currentEpoch + 1) enacted
  -- Known divergence: nothing deletes the @reward@ rows of
  -- stake-deregistered credentials.
  LedgerRestrainedRewards {} -> pure ()
  _ -> pure ()

-- | Per-pool rewards from 'LedgerTotalRewards'. The cardano-ledger
-- shape splits on 'Ledger.RewardType' (leader against member), so
-- 'rewardTypeToSource' maps it to the DB enum.
emitLedgerRewards
  :: ( HasResolver env
     , HasWriter env
     , HasNetwork env
     , MonadReader env m
     , MonadIO m
     )
  => IORef (Map ByteString PoolHashId)
  -> Word64
  -> Map StakeCred (Set Ledger.Reward)
  -> m ()
emitLedgerRewards poolCache spendable rewardsMap = do
  writer <- asks getWriter
  forM_ (Map.toList rewardsMap) $ \(stakeCred, rewardSet) -> do
    saId <- resolveStakeCred (stakeCredHash stakeCred)
    forM_ (Set.toList rewardSet) $ \r -> do
      phId <- memoPoolHash poolCache (poolKeyHashBytes (Ledger.rewardPool r))
      liftIO $ writeReward writer Reward
        { rewardAddrId         = saId
        , rewardType           = rewardTypeToSource (Ledger.rewardType r)
        , rewardAmount         = DbLovelace (fromIntegral (unCoin (Ledger.rewardAmount r)))
        , rewardSpendableEpoch = spendable
        , rewardPoolId         = phId
        , rewardEarnedEpoch    = 0      -- PG fills via the generated column
        }

-- | Pool-deposit refunds from 'LedgerPoolReap' arrive already in the
-- 'Generic.Reward' shape, so they need no 'rewardTypeToSource' step.
emitGenericRewards
  :: ( HasResolver env
     , HasWriter env
     , HasNetwork env
     , MonadReader env m
     , MonadIO m
     )
  => IORef (Map ByteString PoolHashId)
  -> Word64
  -> Map StakeCred (Set Generic.Reward)
  -> m ()
emitGenericRewards poolCache spendable rewardsMap = do
  writer <- asks getWriter
  forM_ (Map.toList rewardsMap) $ \(stakeCred, rewardSet) -> do
    saId <- resolveStakeCred (stakeCredHash stakeCred)
    forM_ (Set.toList rewardSet) $ \r -> do
      phId <- memoPoolHash poolCache (poolKeyHashBytes (Generic.rewardPool r))
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
    saId <- resolveStakeCred (stakeCredHash stakeCred)
    forM_ (Set.toList potSet) $ \pr ->
      liftIO $ writePotReward writer PotReward
        { potRewardAddrId         = saId
        , potRewardType           = Generic.prSource pr
        , potRewardAmount         = DbLovelace (fromIntegral (unCoin (Generic.prAmount pr)))
        , potRewardSpendableEpoch = spendable
        , potRewardEarnedEpoch    = 0      -- PG fills via the generated column
        }

-- | Conway enacted treasury withdrawals. Each withdrawal pays a
-- 'Coin' to an 'AccountAddress', which credits the matching
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
          saId <- resolveStakeCred (accountAddrCred acct)
          liftIO $ writePotReward writer PotReward
            { potRewardAddrId         = saId
            , potRewardType           = RwdTreasury
            , potRewardAmount         = DbLovelace (fromIntegral (unCoin coin))
            , potRewardSpendableEpoch = spendable
            , potRewardEarnedEpoch    = 0
            }

-- | 'resolveAndWritePoolHash' behind a boundary-local cache. The
-- rewards map holds one entry per credential but draws on the far
-- smaller set of registered pools, so the memo turns repeated LSM
-- lookups into map hits.
memoPoolHash
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => IORef (Map ByteString PoolHashId)
  -> ByteString
  -> m PoolHashId
memoPoolHash ref key = do
  cache <- liftIO $ readIORef ref
  case Map.lookup key cache of
    Just phId -> pure phId
    Nothing   -> do
      !phId <- resolveAndWritePoolHash key
      liftIO $ modifyIORef' ref (Map.insert key phId)
      pure phId

-- ---------------------------------------------------------------------------
-- * Key extraction helpers
-- ---------------------------------------------------------------------------

stakeCredHash :: StakeCred -> CredHash
stakeCredHash = \case
  Ledger.KeyHashObj (Ledger.KeyHash h)      -> CredHash (Crypto.hashToBytes h) False
  Ledger.ScriptHashObj (Core.ScriptHash h)  -> CredHash (Crypto.hashToBytes h) True

poolKeyHashBytes :: PoolKeyHash -> ByteString
poolKeyHashBytes (Ledger.KeyHash h) = Crypto.hashToBytes h

accountAddrCred :: Ledger.AccountAddress -> CredHash
accountAddrCred = rewardAddrCredHash . Ledger.serialiseAccountAddress
