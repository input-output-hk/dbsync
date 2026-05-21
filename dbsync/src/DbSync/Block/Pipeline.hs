{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Block processing pipeline for the unified extraction architecture.
--
-- Pre-assigns shared IDs (BlockId, TxId, TxOutId) centrally, builds
-- a 'BlockContext', then runs all enabled extractors. Works identically
-- in both 'IngestChainHistory' and 'FollowingChainTip' — only the
-- 'IdResolver' and 'Writer' implementations carried by the env differ.
module DbSync.Block.Pipeline
  ( -- * Processing
    processBlock
  ) where

import Cardano.Prelude

import qualified Data.Sequence as Seq

import DbSync.Block.Types (BlockEra (..), GenericBlock (..), GenericTx (..), GenericTxOut (..))
import DbSync.Db.Types (DbLovelace (..))
import DbSync.Env (HasNetwork (..))
import DbSync.Extractor
  ( BlockContext (..)
  , ExtractorDef (..)
  , HasExtractors (..)
  , HasLedgerData (..)
  , TxContext (..)
  )
import DbSync.Extractor.Core (mkSlotLeader)
import DbSync.Extractor.SharedDedup
  ( resolveAndWritePoolHash
  , resolveAndWriteStakeAddress
  )
import DbSync.Extractor.UTxO (extractStakeCred)
import DbSync.Db.Schema.Ids (PoolHashId, StakeAddressId)
import DbSync.Phase.Current (HasCurrentPhase (..))
import DbSync.Phase.Ingest.UtxoCache (UtxoTxEntry (..))
import DbSync.Resolver (HasResolver (..), IdResolver (..))
import DbSync.Writer (HasWriter (..))

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
processBlock
  :: ( MonadReader env m
     , HasResolver env
     , HasWriter env
     , HasExtractors env
     , HasNetwork env
     , HasLedgerData env
     , HasCurrentPhase env
     , MonadIO m
     )
  => GenericBlock
  -> m ()
processBlock block = do
  resolver   <- asks getResolver
  extractors <- asks getExtractors
  env        <- ask
  phase      <- liftIO $ getCurrentPhase env
  ledgerData <- liftIO $ getLedgerData env block

  -- For Shelley+ blocks the slot-leader hash IS a pool key hash;
  -- resolve it before extractors run so @slot_leader.pool_hash_id@
  -- can be set without giving @core@ a circular dependency on @pool@.
  mPoolHashId <- resolveSlotLeaderPoolHash block

  let leader = mkSlotLeader mPoolHashId block
  (slId, _isNew) <- liftIO $ resolveSlotLeader resolver (blkSlotLeader block) leader

  prevId  <- liftIO $ resolvePrevBlock resolver (blkPreviousHash block)
  blockId <- liftIO $ assignBlockId resolver

  -- Per-tx and per-output IDs plus the per-output stake-address FK.
  -- Recording each tx in the cache before extractors run lets the
  -- UTxO extractor resolve intra-block inputs (a later tx in the same
  -- block spending an earlier tx's output) without ordering games.
  -- The cache stores (tx_out.id, value) per output so the consumed-by
  -- UPDATE matches by PK rather than (tx_id, index).
  --
  -- The bang on @v@ forces the Word64 before the tuple is built. Without
  -- it the tuple's second field is a thunk that retains its captured
  -- @o :: GenericTxOut@ (and through it the raw address ByteString), so
  -- the cache transitively pins ~1.8 GB of GenericTxOut + ByteString +
  -- ARR_WORDS for every retained tx. Heap profiling traced the leak to
  -- this exact site.
  txCtxs <- forM (blkTxs block) $ \gtx -> do
    txId <- liftIO $ assignTxId resolver
    outIds <- forM (txOutputs gtx) $ \_ -> liftIO $ assignTxOutId resolver
    stakeIds <- forM (txOutputs gtx) resolveOutStakeId
    liftIO $ recordTxOutputs resolver (txHash gtx) UtxoTxEntry
      { uteTxId    = txId
      , uteOutputs = Seq.fromList
          [ let !v = txOutValue o
            in (outId, DbLovelace v)
          | (outId, o) <- zip outIds (txOutputs gtx)
          ]
      }
    pure $ TxContext txId gtx outIds stakeIds

  network <- asks getNetwork
  let ctx = BlockContext
        { bcBlockId              = blockId
        , bcSlotLeaderId         = slId
        , bcSlotLeaderNew        = _isNew
        , bcSlotLeaderPoolHashId = mPoolHashId
        , bcPrevBlockId          = prevId
        , bcGenBlock             = block
        , bcTxs                  = txCtxs
        , bcNetwork              = network
        , bcLedgerData           = ledgerData
        , bcSyncPhase            = phase
        }

  forM_ extractors $ \ext -> pdProcess ext ctx

-- | Resolve the slot leader's pool hash for Shelley+ blocks.
--
-- Byron blocks delegate slot leadership through genesis keys (not pool
-- keys) and EBBs carry a synthetic null leader, so both produce
-- 'Nothing' here. For everything else we dedup-write a pool_hash row.
resolveSlotLeaderPoolHash
  :: (HasResolver env, HasWriter env, MonadReader env m, MonadIO m)
  => GenericBlock -> m (Maybe PoolHashId)
resolveSlotLeaderPoolHash block
  | blkEra block == Byron = pure Nothing
  | otherwise = do
      (phId, _) <- resolveAndWritePoolHash (blkSlotLeader block)
      pure (Just phId)

-- | Pre-resolve the @stake_address@ FK for one tx output.
--
-- Lives here (rather than in the UTxO extractor) so the @utxo@ and
-- @stake_delegation@ extractors stay textually independent — the
-- pipeline is the only place that calls into both.
resolveOutStakeId
  :: ( HasResolver env
     , HasWriter env
     , HasNetwork env
     , MonadReader env m
     , MonadIO m
     )
  => GenericTxOut -> m (Maybe StakeAddressId)
resolveOutStakeId gout =
  case extractStakeCred (txOutAddressRaw gout) of
    Nothing -> pure Nothing
    Just credHash -> Just <$> resolveAndWriteStakeAddress credHash
