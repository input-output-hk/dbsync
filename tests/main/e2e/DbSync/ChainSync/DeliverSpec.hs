{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Atomicity tests for the receiver's block-delivery step.
--
-- The chainsync receiver is killed with an async exception at the
-- Ingest → Follow handoff and on node reconnects, and the
-- ledger-queue write can block for seconds while the worker catches
-- up. 'deliverForwardBlock' therefore commits the two queue writes
-- and the latest-received-point update in a single STM transaction:
-- a kill parked on a full queue must deliver nothing and record
-- nothing. When the pieces were separate writes, a kill landing
-- between them left the block queued but unrecorded — the next
-- session re-requested it and the Follow consumer applied it twice,
-- aborting the app on the @tx@ unique constraint.
--
-- These scenarios pin that all-or-nothing contract deterministically
-- (a full ledger queue parks the delivery thread exactly where the
-- production kill landed), which no black-box app run can do.
module DbSync.ChainSync.DeliverSpec (spec) where

import Cardano.Prelude

import Control.Concurrent.STM
  ( flushTBQueue
  , lengthTBQueue
  , newTBQueueIO
  , newTVarIO
  , readTVarIO
  , writeTBQueue
  )

import Test.Hspec (Spec, beforeAll, describe, expectationFailure, it, shouldBe)

import Ouroboros.Consensus.Cardano.Block (CardanoBlock, StandardCrypto)
import Ouroboros.Network.Block (blockPoint, genesisPoint)

import DbSync.ChainSync.Connection (deliverForwardBlock, deliverRollback)
import DbSync.ChainSync.Msg (ChainSyncMsg (..))
import DbSync.Parser.Types (CardanoPoint)
import DbSync.Test.E2E (conwayConfigDir)
import DbSync.Test.MockChain (forgeNextBlock, withMockChain)

spec :: Spec
spec = describe "ChainSync block delivery atomicity" $
  beforeAll forgeSampleBlock $ do

    it "delivers nothing when killed while parked on a full ledger queue" $ \blk -> do
      mainQueue   <- newTBQueueIO 8
      ledgerQueue <- newTBQueueIO 1
      -- Fill the ledger queue to capacity so the delivery transaction
      -- retries — the exact state the receiver was killed in at the
      -- handoff (worker behind, fan-out write parked).
      atomically $ writeTBQueue ledgerQueue (MsgRollback genesisPoint)
      latestVar <- newTVarIO (Nothing :: Maybe CardanoPoint)

      withAsync (deliverForwardBlock mainQueue (Just ledgerQueue) latestVar blk) $ \a -> do
        -- Long enough for the thread to start and park on the retry;
        -- if it is killed before even starting, the assertions below
        -- still hold, so the test cannot false-fail.
        threadDelay 200_000
        cancel a
        result <- waitCatch a
        case result of
          Left  _  -> pure ()
          Right () ->
            expectationFailure
              "delivery completed despite a full ledger queue"

      -- All-or-nothing: the kill must not leave the block on the main
      -- queue with the point unrecorded (the double-apply bug), nor
      -- advance the point without the block being queued (a gap).
      mainLen <- atomically $ lengthTBQueue mainQueue
      mainLen `shouldBe` 0
      latest <- readTVarIO latestVar
      latest `shouldBe` Nothing

    it "delivers to both queues and records the point together" $ \blk -> do
      mainQueue   <- newTBQueueIO 8
      ledgerQueue <- newTBQueueIO 8
      latestVar   <- newTVarIO Nothing

      deliverForwardBlock mainQueue (Just ledgerQueue) latestVar blk

      mainMsgs   <- atomically $ flushTBQueue mainQueue
      ledgerMsgs <- atomically $ flushTBQueue ledgerQueue
      map (isForwardOf blk) mainMsgs   `shouldBe` [True]
      map (isForwardOf blk) ledgerMsgs `shouldBe` [True]
      latest <- readTVarIO latestVar
      latest `shouldBe` Just (blockPoint blk)

    it "skips the ledger queue when the ledger worker is disabled" $ \blk -> do
      mainQueue <- newTBQueueIO 8
      latestVar <- newTVarIO Nothing

      deliverForwardBlock mainQueue Nothing latestVar blk

      mainMsgs <- atomically $ flushTBQueue mainQueue
      map (isForwardOf blk) mainMsgs `shouldBe` [True]
      latest <- readTVarIO latestVar
      latest `shouldBe` Just (blockPoint blk)

    it "delivers a reorg rollback marker to both queues with the point" $ \blk -> do
      mainQueue   <- newTBQueueIO 8
      ledgerQueue <- newTBQueueIO 8
      latestVar   <- newTVarIO Nothing
      let target = blockPoint blk

      deliverRollback mainQueue (Just ledgerQueue) latestVar False target

      mainMsgs   <- atomically $ flushTBQueue mainQueue
      ledgerMsgs <- atomically $ flushTBQueue ledgerQueue
      map (isRollbackTo target) mainMsgs   `shouldBe` [True]
      map (isRollbackTo target) ledgerMsgs `shouldBe` [True]
      latest <- readTVarIO latestVar
      latest `shouldBe` Just target

    it "only records the point for a confirming rollback" $ \blk -> do
      mainQueue   <- newTBQueueIO 8
      ledgerQueue <- newTBQueueIO 8
      latestVar   <- newTVarIO Nothing
      let target = blockPoint blk

      deliverRollback mainQueue (Just ledgerQueue) latestVar True target

      mainLen   <- atomically $ lengthTBQueue mainQueue
      ledgerLen <- atomically $ lengthTBQueue ledgerQueue
      mainLen   `shouldBe` 0
      ledgerLen `shouldBe` 0
      latest <- readTVarIO latestVar
      latest `shouldBe` Just target

-- | One real forged block. Only its 'blockPoint' identity matters
-- here; the interpreter (and its config files) are released before
-- the scenarios run.
forgeSampleBlock :: IO (CardanoBlock StandardCrypto)
forgeSampleBlock =
  withMockChain conwayConfigDir $ \mc -> forgeNextBlock mc []

isForwardOf :: CardanoBlock StandardCrypto -> ChainSyncMsg -> Bool
isForwardOf blk = \case
  MsgForward b  -> blockPoint b == blockPoint blk
  MsgRollback _ -> False

isRollbackTo :: CardanoPoint -> ChainSyncMsg -> Bool
isRollbackTo target = \case
  MsgRollback p -> p == target
  MsgForward _  -> False
