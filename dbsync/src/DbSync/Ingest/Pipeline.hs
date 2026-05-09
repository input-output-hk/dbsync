{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Block processing pipeline for the unified extraction architecture.
--
-- Pre-assigns shared IDs (BlockId, TxId, TxOutId) centrally, builds
-- a 'BlockContext', then runs all enabled extractors. Works identically
-- in both 'IngestChainHistory' and 'FollowingChainTip' — only the
-- 'IdResolver' and 'Writer' implementations carried by the env differ.
module DbSync.Ingest.Pipeline
  ( -- * Processing
    processBlock
  ) where

import Cardano.Prelude

import Cardano.Ledger.BaseTypes (Network)

import DbSync.Block.Types (BlockEra (..), GenericBlock (..), GenericTx (..), GenericTxOut (..))
import DbSync.Env (HasNetwork (..))
import DbSync.Extractor
  ( BlockContext (..)
  , BlockLedgerData
  , ExtractorDef (..)
  , HasExtractors (..)
  , HasLedgerData (..)
  , HasSyncPhase (..)
  , TxContext (..)
  )
import DbSync.Extractor.Core (mkSlotLeader)
import DbSync.Extractor.SharedDedup
  ( resolveAndWritePoolHash
  , resolveAndWriteStakeAddress
  )
import DbSync.Extractor.UTxO (extractStakeCred)
import DbSync.Db.Schema.Ids (PoolHashId, StakeAddressId)
import DbSync.Phase (SyncPhase)
import DbSync.Resolver (HasResolver (..), IdResolver (..))
import DbSync.Writer (HasWriter (..), Writer)

-- ---------------------------------------------------------------------------
-- * Processing
-- ---------------------------------------------------------------------------

-- | Process a single 'GenericBlock' through all enabled extractors.
--
-- 1. Pre-assigns shared IDs: BlockId, SlotLeaderId, per-tx TxId,
--    per-output TxOutId.
-- 2. Builds a 'BlockContext' containing these IDs.
-- 3. Calls each extractor's 'pdProcess' with the context.
--
-- Extractors are independent — they consume the pre-assigned IDs
-- without depending on each other's execution order.
--
-- Polymorphic over the env so it works in both 'IngestChainHistory' (where
-- the env is 'DbSync.Env.IngestEnv') and 'FollowingChainTip' (where the
-- env will be 'DbSync.Env.FollowEnv' once the SELECT/INSERT resolver +
-- writer land).
processBlock
  :: ( MonadReader env m
     , HasResolver env
     , HasWriter env
     , HasExtractors env
     , HasNetwork env
     , HasLedgerData env
     , HasSyncPhase env
     , MonadIO m
     )
  => GenericBlock
  -> m ()
processBlock block = do
  resolver   <- asks getResolver
  writer     <- asks getWriter
  extractors <- asks getExtractors
  network    <- asks getNetwork
  phase      <- asks getSyncPhase
  env        <- ask
  liftIO $ do
    ledgerData <- getLedgerData env block
    runProcessBlock resolver writer extractors network phase ledgerData block

-- | The pure-IO core of 'processBlock'. Kept separate so the env-pulling
-- wrapper stays trivial and the extractor pipeline (which is hot-path code
-- with mutable refs) doesn't pay any 'ReaderT' overhead.
runProcessBlock
  :: IdResolver IO
  -> Writer IO
  -> [ExtractorDef]
  -> Network
  -> SyncPhase
  -> BlockLedgerData
  -> GenericBlock
  -> IO ()
runProcessBlock resolver writer extractors network phase ledgerData block = do
  -- For Shelley+ blocks the slot-leader hash IS a pool key hash; resolve
  -- it before extractors run so @slot_leader.pool_hash_id@ can be set
  -- without giving @core@ a circular dependency on @pool@.
  mPoolHashId <- resolveSlotLeaderPoolHash resolver writer block

  let leader = mkSlotLeader mPoolHashId block
  (slId, isNew) <- resolveSlotLeader resolver (blkSlotLeader block) leader

  prevId <- resolvePrevBlock resolver (blkPreviousHash block)
  blockId <- assignBlockId resolver

  -- Per-tx and per-output IDs plus the per-output stake-address FK.
  txCtxs <- forM (blkTxs block) $ \gtx -> do
    txId <- assignTxId resolver
    outIds <- forM (txOutputs gtx) $ \_ -> assignTxOutId resolver
    stakeIds <- forM (txOutputs gtx) (resolveOutStakeId network resolver writer)
    pure $ TxContext txId gtx outIds stakeIds

  let ctx = BlockContext
        { bcBlockId              = blockId
        , bcSlotLeaderId         = slId
        , bcSlotLeaderNew        = isNew
        , bcSlotLeaderPoolHashId = mPoolHashId
        , bcPrevBlockId          = prevId
        , bcGenBlock             = block
        , bcTxs                  = txCtxs
        , bcNetwork              = network
        , bcLedgerData           = ledgerData
        , bcSyncPhase            = phase
        }

  forM_ extractors $ \ext ->
    pdProcess ext resolver writer ctx

-- | Resolve the slot leader's pool hash for Shelley+ blocks.
--
-- Byron blocks delegate slot leadership through genesis keys (not pool
-- keys) and EBBs carry a synthetic null leader, so both produce
-- 'Nothing' here. For everything else we dedup-write a pool_hash row.
resolveSlotLeaderPoolHash
  :: IdResolver IO -> Writer IO -> GenericBlock -> IO (Maybe PoolHashId)
resolveSlotLeaderPoolHash resolver writer block
  | blkEra block == Byron = pure Nothing
  | otherwise = do
      (phId, _) <- resolveAndWritePoolHash resolver writer (blkSlotLeader block)
      pure (Just phId)

-- | Pre-resolve the @stake_address@ FK for one tx output.
--
-- Lives here (rather than in the UTxO extractor) so the @utxo@ and
-- @stake_delegation@ extractors stay textually independent — the
-- pipeline is the only place that calls into both.
resolveOutStakeId
  :: Network
  -> IdResolver IO
  -> Writer IO
  -> GenericTxOut
  -> IO (Maybe StakeAddressId)
resolveOutStakeId network resolver writer gout =
  case extractStakeCred (txOutAddressRaw gout) of
    Nothing -> pure Nothing
    Just credHash ->
      Just <$> resolveAndWriteStakeAddress network resolver writer credHash
