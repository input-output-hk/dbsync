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

import DbSync.Block.Types (GenericBlock (..), GenericTx (..))
import DbSync.Extractor (ExtractorDef (..), BlockContext (..), HasExtractors (..), TxContext (..))
import DbSync.Extractor.Core (mkSlotLeader)
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
     , MonadIO m
     )
  => GenericBlock
  -> m ()
processBlock block = do
  resolver   <- asks getResolver
  writer     <- asks getWriter
  extractors <- asks getExtractors
  liftIO $ runProcessBlock resolver writer extractors block

-- | The pure-IO core of 'processBlock'. Kept separate so the env-pulling
-- wrapper stays trivial and the extractor pipeline (which is hot-path code
-- with mutable refs) doesn't pay any 'ReaderT' overhead.
runProcessBlock
  :: IdResolver IO
  -> Writer IO
  -> [ExtractorDef]
  -> GenericBlock
  -> IO ()
runProcessBlock resolver writer extractors block = do
  -- 1. Resolve slot leader (dedup)
  let leader = mkSlotLeader block
  (slId, isNew) <- resolveSlotLeader resolver (blkSlotLeader block) leader

  -- 2. Resolve previous block
  prevId <- resolvePrevBlock resolver (blkPreviousHash block)

  -- 3. Assign block ID
  blockId <- assignBlockId resolver

  -- 4. Assign per-tx TxIds and per-output TxOutIds
  txCtxs <- forM (blkTxs block) $ \gtx -> do
    txId <- assignTxId resolver
    outIds <- forM (txOutputs gtx) $ \_ -> assignTxOutId resolver
    pure $ TxContext txId gtx outIds

  -- 5. Build context
  let ctx = BlockContext
        { bcBlockId       = blockId
        , bcSlotLeaderId  = slId
        , bcSlotLeaderNew = isNew
        , bcPrevBlockId   = prevId
        , bcGenBlock      = block
        , bcTxs           = txCtxs
        }

  -- 6. Run each extractor
  forM_ extractors $ \ext ->
    pdProcess ext resolver writer ctx
