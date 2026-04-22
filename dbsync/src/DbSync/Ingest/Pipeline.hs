{-# LANGUAGE OverloadedStrings #-}

-- | Block processing pipeline for the unified extraction architecture.
--
-- Pre-assigns shared IDs (BlockId, TxId, TxOutId) centrally, builds
-- a 'BlockContext', then runs all enabled extractors. Works identically
-- in both 'IngestChainHistory' and 'FollowingChainTip' — only the
-- resolver and writer implementations differ.
module DbSync.Ingest.Pipeline
  ( -- * Processing
    processBlock
  ) where

import Cardano.Prelude

import DbSync.Block.Types (GenericBlock (..), GenericTx (..))
import DbSync.Extractor (ExtractorDef (..), BlockContext (..), TxContext (..))
import DbSync.Extractor.Core (mkSlotLeader)
import DbSync.Resolver (IdResolver (..))
import DbSync.Writer (Writer)

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
  :: IdResolver IO
  -> Writer IO
  -> [ExtractorDef]
  -> GenericBlock
  -> IO ()
processBlock resolver writer extractors block = do
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
