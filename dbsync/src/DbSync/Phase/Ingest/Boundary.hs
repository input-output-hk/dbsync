{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Epoch-boundary handling for the Ingest consumer.
--
-- The consumer's per-block loop in "DbSync.Phase.Ingest.Consumer"
-- detects an epoch transition by comparing 'sdEpochNo' against the
-- previous block's. When it crosses, control passes here.
--
-- 'handleEpochBoundary' runs a pipelined cascade — flush COPY,
-- snapshot per-epoch buffers, await the tx-out worker, advance
-- @sync_state@ for the /previous/ pending epoch, enqueue the
-- just-finished one, reopen the loader stream, compact the LSM
-- tables, and emit the per-epoch summary line. The pipelining means
-- @sync_state@ always lags by one epoch behind the consumer.
module DbSync.Phase.Ingest.Boundary
  ( -- * Loop state
    ConsumerLoopState (..)
  , newConsumerLoopState
  , PendingBoundary (..)

    -- * Boundary handler
  , handleEpochBoundary

    -- * LedgerWorker coordination
  , waitForApplyResultAt
  , readBoundaryApplyResult
  , flushPendingDeposits

    -- * Helpers
  , compactIngestStores
  , renderEpochSummary
  , renderUtxoHitRate
  , renderDedupCounts
  , sampleHeapBytes
  , fmtBytes
  , fmtDuration
  ) where

import Cardano.Prelude

import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))

import qualified Control.Concurrent.Class.MonadSTM.Strict as Strict
import Control.Concurrent.STM (readTVarIO)
import Control.Concurrent.STM.TBQueue (readTBQueue)
import Control.Tracer (traceWith)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Strict.Maybe as SMaybe
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import GHC.Stats (RTSStats (..), GCDetails (..), getRTSStats, getRTSStatsEnabled)
import System.Mem (performMajorGC)
import Text.Printf (printf)

import DbSync.AppM (IngestM, runAppM)
import DbSync.App.Config.Types (LedgerConfig (..), SyncConfig (..))
import DbSync.App.Env (CoreEnv (..), HasConfig (..), IngestEnv (..))
import DbSync.Db.Loader (LoaderStream (..))
import DbSync.Db.Schema.EpochSyncStats (EpochSyncStats (..))
import DbSync.Extractor (ExtractState (..))
import DbSync.Phase.Ingest.Counter (IdCounters)
import DbSync.Phase.Ingest.DedupStore (DedupStores, dedupStoreSizes)
import qualified DbSync.Phase.Ingest.DedupStore as DedupStore
import DbSync.Phase.Ingest.LsmSession (LsmSession)
import DbSync.Phase.Ingest.PipelineStats (emptyPipelineStats)
import qualified DbSync.Phase.Ingest.UtxoStore as UtxoStore
import DbSync.Phase.Type (SyncPhase (..), renderPhase)
import DbSync.SyncState.Manager (mkBoundarySyncStateRow)
import DbSync.SyncState.Row (ControlConnection, writeSyncState)
import DbSync.Trace (HasTracer (..))
import DbSync.Trace.Replay (ReplayLogState (..))
import DbSync.Trace.Timing (fmtCount, fmtF2)
import DbSync.Trace.Types (LogMsg (..), Severity (..))
import DbSync.Trace.Watchdog (Watchdog, setConsumerNote)
import DbSync.Worker.Ledger.DepositAccumulator (drainCompletedEpochs, flushEpochParams)
import DbSync.Worker.Ledger.Types
  ( ApplyResult (..)
  , HasLedgerEnv (..)
  , LedgerEnv (..)
  )
import DbSync.Worker.TxOut.AddressBuffer (takeAndReset)
import qualified DbSync.Worker.TxOut.ConsumedByBuffer as ConsumedByBuffer
import DbSync.Worker.TxOut.Worker
  ( TxOutJob (..)
  , awaitTxOutDrained
  , enqueueTxOutJob
  , readAddressIdCounter
  )
import DbSync.Resolver (IdResolver (..))
import DbSync.Writer (Writer (..))
import DbSync.StateQuery (sdSlotNo)

-- ---------------------------------------------------------------------------
-- * Loop state
-- ---------------------------------------------------------------------------

-- | Snapshot of one finished epoch, held until the resolver has
-- caught up. A job for epoch @N@ is enqueued at the @N → N+1@
-- boundary; @sync_state@ for @N@ only advances at the @N+1 → N+2@
-- boundary, once the tx-out worker has resolved every
-- @tx_out.address_id@ FK for @N@. On a clean exit at the rollback
-- boundary the consumer drains the final queued job and writes the
-- snapshot held here.
data PendingBoundary = PendingBoundary
  { pbEpoch       :: !EpochNo
  , pbLastSlot    :: !Word64
  , pbLastBlockNo :: !Word64
  , pbLastHash    :: !ByteString
  , pbCounters    :: !IdCounters
  }

-- | Mutable state threaded through the consumer loop. Lives for one
-- 'runConsumer' invocation.
data ConsumerLoopState = ConsumerLoopState
  { clsPrevEpoch       :: !(IORef (Maybe EpochNo))
    -- ^ Epoch of the most recently processed block; 'Nothing' until
    -- the first block lands. Boundary detection compares against
    -- this.
  , clsBlockCount      :: !(IORef Word64)
    -- ^ Blocks processed in the current epoch; reset at each
    -- boundary. Feeds the per-epoch @blk/s@ rate.
  , clsEpochStart      :: !(IORef UTCTime)
    -- ^ Wall-clock start of the current epoch's window.
  , clsLastBlock       :: !(IORef (Maybe (Word64, Word64, ByteString)))
    -- ^ @(slot, blockNo, hash)@ of the most recently processed
    -- block; the resume point captured at each boundary.
  , clsPendingBoundary :: !(IORef (Maybe PendingBoundary))
    -- ^ Snapshot of the previous epoch's boundary state, held until
    -- the resolver catches up.
  , clsReplay          :: !(IORef ReplayLogState)
    -- ^ Replay-progress state machine; 'ReplayPending' on a resume
    -- with a replay window, 'NoReplay' otherwise.
  }

-- | Allocate a fresh 'ConsumerLoopState'. The replay machine is
-- seeded based on whether the boot had a replay window.
newConsumerLoopState :: Maybe SlotNo -> IO ConsumerLoopState
newConsumerLoopState bootSlot = do
  now <- getCurrentTime
  ConsumerLoopState
    <$> newIORef Nothing
    <*> newIORef 0
    <*> newIORef now
    <*> newIORef Nothing
    <*> newIORef Nothing
    <*> newIORef (case bootSlot of
                    Just _  -> ReplayPending
                    Nothing -> NoReplay)

-- ---------------------------------------------------------------------------
-- * Boundary handler
-- ---------------------------------------------------------------------------

-- | Run the 9-step boundary cascade for the @prev → blockEpoch@
-- transition. Called from the consumer when @prev /= blockEpoch@.
--
-- The steps, in order:
--
--   1. Commit the loader stream — tx_outs durable, address_id NULL.
--   2. Snapshot the address buffer (+ optional consumed-by buffer).
--   3. Await the tx-out worker draining the /previous/ boundary's job.
--   4. Flush the ledger worker's pending epoch_param deposits.
--   5. Advance @sync_state@ for the lagging epoch (now fully resolved).
--   6. Enqueue the just-finished epoch's resolve job to the worker.
--   7. Stash the snapshot for the /next/ boundary's @sync_state@ write.
--   8. Reopen the loader stream for the next epoch.
--   9. Compact the LSM tables (caps active runs + warms restart).
--
-- Followed by the per-epoch summary log, optional dedup-debug log,
-- and a major GC on epochs above 10s wall-clock.
handleEpochBoundary
  :: ConsumerLoopState
  -> EpochNo        -- ^ epoch just completed
  -> SlotNo         -- ^ boundary block's slot
  -> IngestM ()
handleEpochBoundary cls prev slot = do
  ie       <- ask
  let tracer        = getTracer ie
      txOutWorker   = ieTxOutWorker ie
      mConsumedBuf  = ieConsumedByBuffer ie
      loaderStream  = ieLoaderStream ie
      watchdog      = ieWatchdog ie
      ctrlConn      = ieControlConnection ie
      hasLedger     = ieHasLedgerEnv ie
      addressBuffer = ieAddressBuffer ie
      dedupStores   = ieDedupStores ie
      lsm           = ieLsmSession ie
      utxoStore     = ieUtxoStore ie
      resolver      = ieResolver ie
      writer        = ieWriter ie
      statsRef      = iePipelineStats ie
      coreEnv       = ieCore ie
      boundaryVar   = ieRollbackBoundary ie
      securityParam = ceSecurityParam coreEnv
      minSev        = ceMinSeverity coreEnv
      ledgerEnabled = lcEnabled (scLedger (getConfig ie))
      schemaVersion = 1 :: Int

  epochStart <- liftIO $ readIORef (clsEpochStart cls)
  blockCount <- liftIO $ readIORef (clsBlockCount cls)
  mLastBlock <- liftIO $ readIORef (clsLastBlock cls)
  counters   <- liftIO $ esIdCounters <$> readIORef (ieExtractState ie)

  -- Steps 1–4: drain the existing pipeline so the resolver can run
  -- in parallel with the next epoch's ingest.
  (buf, mConsumedSnap) <- liftIO $ do
    setConsumerNote watchdog "consumer: lsCommit (flushing loader stream)"
    lsCommit loaderStream

    setConsumerNote watchdog "consumer: takeAndReset addressBuffer"
    b <- takeAndReset addressBuffer

    cb <- case mConsumedBuf of
      Just ref -> Just <$> ConsumedByBuffer.takeAndReset ref
      Nothing  -> pure Nothing

    setConsumerNote watchdog "consumer: awaitTxOutDrained (epoch N-1)"
    awaitTxOutDrained txOutWorker

    setConsumerNote watchdog "consumer: flushEpochParams"
    flushPendingDeposits hasLedger prev slot ctrlConn
    pure (b, cb)

  -- Step 5: advance @sync_state@ for the previously snapshotted
  -- epoch. The address counter reflects rows just drained in step 3.
  liftIO $ setConsumerNote watchdog "consumer: writeSyncState (lagging)"
  addressIdCounter <- liftIO $ readAddressIdCounter txOutWorker
  mPending         <- liftIO $ readIORef (clsPendingBoundary cls)
  for_ mPending $ \pb ->
    writeSyncState $
      mkBoundarySyncStateRow
        (pbLastSlot pb) (pbLastBlockNo pb) (pbLastHash pb)
        (pbCounters pb) addressIdCounter
        schemaVersion ledgerEnabled

  -- Step 6: queue the just-finished epoch for the worker.
  liftIO $ do
    setConsumerNote watchdog "consumer: enqueueTxOutJob"
    enqueueTxOutJob txOutWorker (TxOutJob prev buf mConsumedSnap)

  -- Step 7: stash the snapshot for the next boundary.
  liftIO $ for_ mLastBlock $ \(lastSlot, lastBlockNo, lastHash) ->
    writeIORef (clsPendingBoundary cls) $ Just PendingBoundary
      { pbEpoch       = prev
      , pbLastSlot    = lastSlot
      , pbLastBlockNo = lastBlockNo
      , pbLastHash    = lastHash
      , pbCounters    = counters
      }

  -- Step 8: reopen loader streams for the next epoch.
  liftIO $ do
    setConsumerNote watchdog "consumer: lsReopen"
    lsReopen loaderStream
    setConsumerNote watchdog "consumer: post-commit"

  -- Step 9: compact the LSM-backed Ingest tables. Caps active run
  -- count (and open fds) and produces the warm-up state for a
  -- resumed boot.
  liftIO $ compactIngestStores watchdog utxoStore dedupStores lsm

  -- Per-epoch stats row + summary log.
  epochEnd <- liftIO getCurrentTime
  let elapsed   = diffUTCTime epochEnd epochStart
      elapsedSec = realToFrac elapsed :: Double
      blocksPerSec
        | elapsed > 0 = fromIntegral blockCount / realToFrac elapsed
        | otherwise   = 0

  essId <- liftIO $ assignEpochSyncStatsId resolver
  liftIO $ writeEpochSyncStats writer essId EpochSyncStats
    { epochSyncStatsEpochNo         = unEpochNo prev
    , epochSyncStatsBlocksProcessed = blockCount
    , epochSyncStatsBlocksPerSec    = blocksPerSec
    , epochSyncStatsElapsedSec      = elapsedSec
    , epochSyncStatsSyncedAt        = epochEnd
    , epochSyncStatsPhase           = renderPhase IngestChainHistory
    }

  when (elapsedSec > 10.0) $ liftIO performMajorGC

  mBoundary  <- liftIO $ readTVarIO boundaryVar
  storeStats <- liftIO $ UtxoStore.readStoreStats utxoStore

  let summary = renderEpochSummary prev blockCount elapsedSec blocksPerSec
                  storeStats mBoundary securityParam
                  (fmap (\(_, b, _) -> b) mLastBlock)

  liftIO $ traceWith tracer $ LogMsg Info "Ingest" summary Nothing

  -- Dedup-store size + heap diagnostic. Gated on Debug because
  -- sampling 'getRTSStats' isn't free.
  when (minSev <= Debug) $ do
    dedupCounts <- liftIO $ dedupStoreSizes dedupStores
    heapInfo    <- liftIO sampleHeapBytes
    let heapText = case heapInfo of
          Just live -> " | heap=" <> fmtBytes live
          Nothing   -> ""
    liftIO $ traceWith tracer $ LogMsg Debug "Dedup"
      ( "Epoch " <> show (unEpochNo prev)
        <> " | " <> renderDedupCounts dedupCounts
        <> heapText
      ) Nothing

  -- Reset for the next epoch.
  liftIO $ do
    modifyIORef' statsRef (const emptyPipelineStats)
    writeIORef (clsBlockCount cls) 0
    writeIORef (clsEpochStart cls) epochEnd

-- ---------------------------------------------------------------------------
-- * LedgerWorker coordination
-- ---------------------------------------------------------------------------

-- | Block until 'LedgerWorker' has produced any 'ApplyResult' at or
-- past @targetSlot@. Used as a slot-reached barrier; callers that
-- need the boundary's 'ApplyResult' itself use
-- 'readBoundaryApplyResult'.
waitForApplyResultAt :: LedgerEnv -> SlotNo -> IO ApplyResult
waitForApplyResultAt lenv targetSlot = Strict.atomically $ do
  mAR <- Strict.readTVar (leLatestApplyResult lenv)
  case mAR of
    SMaybe.Just ar
      | sdSlotNo (apSlotDetails ar) >= targetSlot -> pure ar
    _ -> retry

-- | Take the next boundary 'ApplyResult' from the worker's FIFO,
-- blocking until one is available. Each call consumes exactly one
-- boundary; pair every consumer-side boundary detection with one
-- 'readBoundaryApplyResult'.
readBoundaryApplyResult :: LedgerEnv -> IO ApplyResult
readBoundaryApplyResult lenv =
  Strict.atomically (readTBQueue (leBoundaryApplyResults lenv))

-- | Wait for the worker to catch up to @slot@, drain every
-- accumulated deposit-param entry at or before @prev@, and flush
-- them to @epoch_param_pending@. No-op when the ledger feature is
-- disabled.
flushPendingDeposits
  :: HasLedgerEnv
  -> EpochNo            -- ^ just-completed epoch (drain watermark)
  -> SlotNo             -- ^ boundary block slot (worker catch-up target)
  -> ControlConnection
  -> IO ()
flushPendingDeposits hasLedger prev slot ctrl = case hasLedger of
  LedgerDisabled _   -> pure ()
  LedgerEnabled lenv -> do
    _ <- waitForApplyResultAt lenv slot
    completed <- drainCompletedEpochs (leDepositAccumulator lenv) prev
    runAppM ctrl (flushEpochParams completed)

-- ---------------------------------------------------------------------------
-- * Helpers
-- ---------------------------------------------------------------------------

-- | Snapshot each LSM-backed Ingest table and reopen it from the
-- snapshot. Bounds active run count and seeds the warm-up state for
-- a resumed boot.
compactIngestStores
  :: Watchdog
  -> UtxoStore.UtxoStore
  -> DedupStores
  -> LsmSession
  -> IO ()
compactIngestStores watchdog utxoStore dedupStores lsm = do
  setConsumerNote watchdog "consumer: utxoStore compact"
  UtxoStore.compactUtxoStore utxoStore lsm
  setConsumerNote watchdog "consumer: dedupStore compact"
  DedupStore.compactDedupStore (DedupStore.dstPoolHash     dedupStores) lsm
  DedupStore.compactDedupStore (DedupStore.dstStakeAddress dedupStores) lsm
  DedupStore.compactDedupStore (DedupStore.dstSlotLeader   dedupStores) lsm
  DedupStore.compactDedupStore (DedupStore.dstMultiAsset   dedupStores) lsm
  DedupStore.compactDedupStore (DedupStore.dstScriptHash   dedupStores) lsm

-- | Build the operator-facing per-epoch summary line:
-- @"Epoch N | M blk in Ts (R blk/s) [| utxo HR=…%] [| (~P%)]"@.
renderEpochSummary
  :: EpochNo
  -> Word64                   -- ^ blocks this epoch
  -> Double                   -- ^ elapsed seconds
  -> Double                   -- ^ blocks per second
  -> UtxoStore.StoreStats     -- ^ for the hit-rate segment
  -> Maybe BlockNo            -- ^ rollback boundary
  -> Word64                   -- ^ security parameter @k@
  -> Maybe Word64             -- ^ current block number (for the % segment)
  -> Text
renderEpochSummary prev blockCount elapsedSec blocksPerSec storeStats
                   mBoundary securityParam mCurBlock =
  "Epoch " <> show (unEpochNo prev)
    <> " | " <> fmtCount blockCount <> " blk in " <> fmtDuration elapsedSec
    <> " (" <> show (round blocksPerSec :: Int) <> " blk/s)"
    <> renderUtxoHitRate storeStats
    <> renderBoundaryPercent mBoundary securityParam mCurBlock

-- | Inline copy of 'DbSync.Phase.Ingest.Consumer.renderBoundaryPercent'
-- used by 'renderEpochSummary' so the public version stays in
-- "DbSync.Phase.Ingest.Consumer" (where the test suite pins its
-- location).
renderBoundaryPercent :: Maybe BlockNo -> Word64 -> Maybe Word64 -> Text
renderBoundaryPercent (Just (BlockNo boundary)) k (Just curBlock)
  | tip > 0 =
      let raw     = (fromIntegral curBlock / fromIntegral tip :: Double) * 100
          clamped = max 0 (min 100 raw)
      in " | [" <> fmtF2 clamped <> "%]"
  where
    tip = boundary + k
renderBoundaryPercent _ _ _ = ""

-- | Render the UtxoStore lifetime hit rate as @" | utxo HR=X.YY%"@.
-- Empty until the first lookup so the boot epoch isn't tagged
-- @0.00%@.
renderUtxoHitRate :: UtxoStore.StoreStats -> Text
renderUtxoHitRate s
  | looked == 0 = ""
  | otherwise   =
      let pct = fromIntegral (UtxoStore.ssHits s) * 100
              / fromIntegral looked :: Double
      in " | utxo HR=" <> fmtF2 pct <> "%"
  where
    looked = UtxoStore.ssHits s + UtxoStore.ssMisses s

-- | Render @[(name, count)]@ as @"name=N1,234 …"@.
renderDedupCounts :: [(Text, Int)] -> Text
renderDedupCounts = Text.intercalate " " . map one
  where
    one (n, c) = n <> "=" <> fmtCount c

-- | Live bytes after the most recent GC, sampled at epoch
-- boundaries. Requires @+RTS -T -RTS@; returns 'Nothing' otherwise.
sampleHeapBytes :: IO (Maybe Word64)
sampleHeapBytes = do
  enabled <- getRTSStatsEnabled
  if enabled
    then Just . gcdetails_live_bytes . gc <$> getRTSStats
    else pure Nothing

-- | Render a byte count as a short human-readable string.
fmtBytes :: Word64 -> Text
fmtBytes b
  | b >= gib  = Text.pack (printf "%.1fGB" (fromIntegral b / fromIntegral gib :: Double))
  | b >= mib  = Text.pack (printf "%dMB"   (b `div` mib))
  | b >= kib  = Text.pack (printf "%dKB"   (b `div` kib))
  | otherwise = show b <> "B"
  where
    kib, mib, gib :: Word64
    kib = 1024
    mib = 1024 * 1024
    gib = 1024 * 1024 * 1024

-- | Format seconds as a coarse human-readable duration. Unlike
-- 'DbSync.Trace.Timing.fmtDuration' the sub-minute case is rounded
-- to whole seconds, keeping the per-epoch summary line compact.
fmtDuration :: Double -> Text
fmtDuration secs
  | secs < 60   = show (round secs :: Int) <> "s"
  | secs < 3600 =
      let t = round secs :: Int
      in show (t `div` 60) <> "m " <> show (t `mod` 60) <> "s"
  | otherwise   =
      let t = round secs :: Int
      in show (t `div` 3600) <> "h " <> show ((t `mod` 3600) `div` 60) <> "m"
